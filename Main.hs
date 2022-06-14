import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.StreamDecoder
import Control.Monad.IO.Class (MonadIO (..))
import System.Environment (getArgs)
import System.IO (openTempFile, hClose)
import System.Directory (removeFile, getCurrentDirectory)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.Text (Text, pack, unpack)
import System.Process
import Control.Monad (liftM, void, mzero)
import MDSem
import MDBuff
import System.IO (openBinaryFile, Handle, IOMode(..), hClose, hGetBuf, hIsEOF)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import Foreign.Marshal.Alloc (callocBytes)
import Sound.OpenAL
import qualified Data.Map as Map (fromList, (!?))

data MDData = MDData Source [Buffer] Handle MDBuffT

type PlayFileT = FilePath -> MDMeta -> IO () -> ReaderT MDOpts IO ()

data MDOpts = MDOpts { buffNum      :: Int
                     , buffSize     :: Int
                     , format       :: String
                     , playlist     :: [(Int, FilePath)]
                     , plistLen     :: Int
                     , allsem       :: MDSemT
                     }

data MDMeta = MDMeta { channels     :: Int
                     , bps          :: Int
                     , sampleRate   :: Int
                     , duration     :: Double
                     , title        :: Text
                     , artist       :: Text
                     }

data MDDuration = MDDuration { hours    :: Int
                             , minutes  :: Int
                             , seconds  :: Int
                             } deriving (Eq)

instance Show MDDuration where
    show (MDDuration 0 0 s) = show s ++ "s"
    show (MDDuration 0 m s) = show m ++ "m" ++ show (MDDuration 0 0 s)
    show (MDDuration h m s) = show h ++ "h" ++ show (MDDuration 0 m s)

getDurationPretty :: Double -> MDDuration
getDurationPretty sec = MDDuration hrs mins secs
    where hrs = floor $ sec / 3600
          mins = floor $ (sec - fromIntegral (hrs * 3600)) / 60
          secs = floor $ sec - fromIntegral (hrs * 3600 + mins * 60)

getDuration :: String -> IO Double
getDuration fname = runFlacMeta defaultMetaSettings fname $ do
    duration <- retrieve (Duration)
    return duration

getMetadata :: FilePath -> IO MDMeta
getMetadata fname = runFlacMeta defaultMetaSettings fname $ do
    ch      <- retrieve (Channels)
    bps     <- retrieve (BitsPerSample)
    sr      <- retrieve (SampleRate)
    dur     <- retrieve (Duration)
    tit     <- retrieve (VorbisComment Title)
    art     <- retrieve (VorbisComment Artist)
    return $ MDMeta (fromIntegral ch) (fromIntegral bps) (fromIntegral sr)
                    dur (getMetaString tit) (getMetaString art)

mapMeta :: String -> MDMeta -> String
mapMeta str meta = case str of
    "%c"    -> show $ channels meta
    "%b"    -> show $ bps meta
    "%s"    -> show $ sampleRate meta
    "%d"    -> show $ getDurationPretty $ duration meta
    "%t"    -> unpack $ title meta
    "%a"    -> unpack $ artist meta

formatMeta :: String -> MDMeta -> String
formatMeta part meta
    | length part < 2   = part
    | otherwise         = case head part of
                '%'     -> let part'    = drop 2 part
                               fmt      = take 2 part
                           in (mapMeta fmt meta) ++ (formatMeta part' meta)

                _       -> let (x:xs)   = part
                           in x:(formatMeta xs meta)

defaultFormat :: String
defaultFormat = "%a - %t\n%d"

getMetaString :: MetaType VorbisComment -> Text
getMetaString Nothing = pack "Unknown"
getMetaString (Just x) = x

printMeta :: String -> IO ()
printMeta fname = runFlacMeta defaultMetaSettings fname $ do
    retrieve (VorbisComment Title) >>= liftIO . print . getMetaString
    retrieve (Duration) >>= liftIO . print . getDurationPretty

getBufferDuration :: MDMeta -> Int -> Double
getBufferDuration meta size = (8 * fromIntegral size)
        / ((fromIntegral ch) * (fromIntegral bs) * (fromIntegral sr))
        where ch = channels meta
              bs = bps meta
              sr = sampleRate meta

decodeFile :: String -> FilePath -> IO ()
decodeFile fname fout = decodeFlac defaultDecoderSettings fname fout

getTemp :: IO FilePath
getTemp = do
    (fpath, handle) <- openTempFile "/tmp" "XXX"
    hClose handle
    return fpath

playFilePacat :: FilePath -> IO ()
playFilePacat fpath = do
        (_, Just hout, _, _) <- createProcess (proc "cat" [ fpath ]) { std_out = CreatePipe }
        (_, _, _, ph) <- createProcess (proc "pacat" [] ) { std_in = UseHandle hout }
        void $ waitForProcess ph

forkIOUntil :: Bool -> IO () -> IO (Maybe ThreadId)
forkIOUntil True _ = do
    return Nothing
forkIOUntil False cb = do
    tid <- forkIO cb
    return (Just tid)

startDecoding' :: [(Int, FilePath)] -> Maybe MDSemT -> PlayFileT
               -> ReaderT MDOpts IO ()
startDecoding' [] oldsem _ = return ()
startDecoding' ((no, fname):xs) oldsem pf =
    ask >>= \opts -> liftIO $ do
        meta    <- getMetadata fname
        sem     <- mdInitCond
        fpath   <- getTemp
        decodeFile fname fpath
        cont    <- forkIOUntil (length xs == 0) $ do
                        threadDelay . ((10^6) *)
                                    . floor
                                    . (0.75 *) . duration $ meta
                        runReaderT (startDecoding' xs (Just sem) pf)
                                   opts

        mdWaitCondMaybe oldsem

        runReaderT (pf fpath meta $ putStrLn
                                  . formatMeta (format opts)
                                  $ meta) opts
        mdSignalCond sem
        removeFile fpath
        void $ maybe (mdSignalCond (allsem opts)) (\_ -> return ()) cont

startDecoding :: PlayFileT -> ReaderT MDOpts IO ()
startDecoding pf = ask >>= \opts -> startDecoding' (playlist opts) Nothing pf

obtainResource :: IO (Maybe a) -> String -> IO () -> MaybeT IO a
obtainResource res str act = liftIO res >>= maybe act' return
    where act' = do
            liftIO $ do
                putStrLn str
                act
            mzero

deinitOpenAL :: Device -> Context -> Source -> [Buffer] -> IO ()
deinitOpenAL dev ctx src bfs = do
    deleteObjectName src
    deleteObjectNames bfs
    destroyContext ctx
    void $ closeDevice dev

withOpenAL :: (Source -> [Buffer] -> ReaderT MDOpts IO ()) -> ReaderT MDOpts IO ()
withOpenAL action = ask >>= \opts -> liftIO $ void $ runMaybeT $ do
    dev <- obtainResource (openDevice Nothing)
                          "Could not open device."
                          mzero
    ctx <- obtainResource (createContext dev [])
                          "Could not open context."
                          (void $ closeDevice dev)
    liftIO $ do
        currentContext $= Just ctx
        src     <- genObjectName                    :: IO Source
        bfs     <- genObjectNames (buffNum opts)    :: IO [Buffer]
        runReaderT (action src bfs) opts
        deinitOpenAL dev ctx src bfs

metaToOpenALFormat :: MDMeta -> Format
metaToOpenALFormat meta =
    case bps meta of
        8   -> case channels meta of
                1   -> Mono8
                2   -> Stereo8
        16  -> case channels meta of
                1   -> Mono16
                2   -> Stereo16

loadBuffs' :: Int -> MDData -> MDMeta -> Int -> Int -> IO MDBuffT
loadBuffs' bfn mddata meta cnt bfsize
    | cnt == 0      = return mdseq
    | otherwise     =
        hIsEOF hnd >>= \eof -> case eof of
            True    -> return mdseq
            False   -> do
                ptr           <- callocBytes bfsize
                buffCount     <- hGetBuf hnd ptr bfsize

                let memReg    = MemoryRegion ptr (fromIntegral buffCount)
                let mdNewSeq  = mdBuffAppend mdseq memReg
                let mdNewData = MDData src bfs hnd mdNewSeq

                bufferData (bfs !! bfn) $= BufferData memReg
                                                (metaToOpenALFormat meta)
                                                (fromIntegral $ sampleRate meta)

                case buffCount < bfsize of
                      True        -> return mdNewSeq
                      False       -> return
                                 =<< loadBuffs' (bfn + 1) mdNewData meta
                                                (cnt - 1) bfsize

    where (MDData src bfs hnd mdseq) = mddata

loadBuffs :: MDData -> MDMeta -> Int -> Int -> IO MDBuffT
loadBuffs = loadBuffs' 0

playFile :: Source -> [Buffer] -> FilePath -> MDMeta -> IO ()
         -> ReaderT MDOpts IO ()
playFile src bfs fpath meta onPlay = ask >>= \opts -> liftIO $ do
        hnd     <- openBinaryFile fpath ReadMode
        let md  = (MDData src bfs hnd mdBuffInit)
        mdbfs   <- loadBuffs md meta (buffNum opts) (buffSize opts)
        let dl  = floor $ 10^6 * 0.5 * (getBufferDuration meta (buffSize opts))
        onPlay
        queueBuffers src (take (mdBuffSize mdbfs) bfs)
        play [src]
        mdBuffDropAll =<< playLoop src hnd dl meta (buffSize opts) mdbfs
        void $ hClose hnd

checkState :: Source -> IO ()
checkState src = sourceState src >>= \st -> case st of
        Stopped     -> play [src]
        _           -> return ()

dropBuffers :: MDBuffT -> IO MDBuffT
dropBuffers mdseq =
    mdBuffDrop mdseq >>= \sq -> case sq of
        Nothing         -> return mdseq
        Just mdnewseq   -> return mdnewseq

waitUntilDone :: Source -> Int -> IO ()
waitUntilDone src delay = sourceState src >>= \st -> case st of
        Playing     -> do
                    threadDelay delay
                    waitUntilDone src delay
        Stopped     -> return ()
        _           -> return ()

playLoop :: Source -> Handle -> Int -> MDMeta -> Int -> MDBuffT -> IO MDBuffT
playLoop src hnd delay meta bfsize mdseq = do
    threadDelay delay
    buffersProcessed src >>= \proc -> case proc > 0 of
        True    -> do
            bfs         <- unqueueBuffers src proc
            let md      = MDData src bfs hnd mdseq
            mdNewSeq    <- loadBuffs md meta (length bfs) bfsize

            let addedBuffCnt = (mdBuffSize mdNewSeq) - (mdBuffSize mdseq)
            case addedBuffCnt == 0 of
                True    -> do
                    waitUntilDone src delay
                    nproc <- buffersProcessed src
                    unqueueBuffers src nproc
                    return mdNewSeq

                False   -> do
                    queueBuffers src (take addedBuffCnt bfs)
                    checkState src
                    return =<< playLoop src hnd delay meta bfsize
                           =<< dropBuffers mdNewSeq

        False   -> do
            checkState src
            return =<< playLoop src hnd delay meta bfsize mdseq

optList = [ ('n', ( (\opts x -> opts { buffNum = read x :: Int } )
            , "number of buffers")
            )
          , ('s', ( (\opts x -> opts { buffSize = read x :: Int } )
            , "buffer size")
            )
          , ('f', ( (\opts x -> opts { format = x } )
            , "text format")
            )
          , ('l', ( (\opts _ -> opts )
            , "open playlist file")
            )
          ]

optMap = Map.fromList optList

getOpts' :: Maybe Char -> MDOpts -> [String] -> IO (Maybe MDOpts)
getOpts' Nothing part [] = return $ Just part
getOpts' _ _ [] = return Nothing
getOpts' Nothing part (x:xs)
    | head x /= '-' || length x /= 2    = return Nothing
    | otherwise                         = return
                                        =<< getOpts' (Just (x !! 1)) part xs
getOpts' (Just ch) opts (x:xs) = case ch of
    'l'     -> return =<< (\pl ->
        let newpl = zip [1..] ((snd . unzip . playlist $ opts) ++ pl)
        in getOpts' Nothing (opts { playlist = newpl
                                  , plistLen = length newpl
                                  }) xs) =<< liftM lines (readFile x)

    _       -> case optMap Map.!? ch of
        Just val    -> return =<< getOpts' Nothing ((fst val) opts x) xs
        Nothing     -> return Nothing

getOpts :: MDOpts -> [String] -> IO (Maybe MDOpts)
getOpts = getOpts' Nothing

main :: IO ()
main = getArgs >>= \args -> do
            let pl      = zip [1..]
                        . takeWhile (\x -> head x /= '-')
                        $ args
            let pllen   = length pl
            let opts'   = drop pllen args

            allsem      <- mdInitCond
            let defOpts = MDOpts 4 4096 defaultFormat pl pllen allsem

            void $ liftIO $ runMaybeT $ do
                opts <- obtainResource (getOpts defOpts opts')
                        "Invalid command line arguments."
                        mzero

                liftIO $ putStrLn $ concat [ "buffSize = "
                                           , show $ buffSize opts
                                           , ", "
                                           , "buffNum = "
                                           , show $ buffNum opts
                                           , ", "
                                           , "format = "
                                           , format opts
                                           ]

                void $ liftIO $ (flip runReaderT) opts $
                    withOpenAL $ \src bfs -> do
                        startDecoding (playFile src bfs)
                        liftIO $ mdWaitCond allsem
