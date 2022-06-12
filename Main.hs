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
import Foreign.Marshal.Alloc (callocBytes)
import Sound.OpenAL

buffNum = 4
buffSize = 4096

data MDData = MDData Source [Buffer] Handle MDBuffT

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

forkIOCond :: [(Int, String)] -> IO () -> IO (Maybe ThreadId)
forkIOCond [] _ = do
    return Nothing
forkIOCond _ cb = do
    tid <- forkIO cb
    return (Just tid)

startDecoding :: Int -> [(Int, String)] -> MDSemT -> Maybe MDSemT
                     -> (FilePath -> MDMeta -> IO () -> IO ()) -> IO ()
startDecoding _ [] _ _ _ = return ()
startDecoding cnt (x@(no, fname):xs) allsem oldsem playf = do
            meta    <- getMetadata fname
            sem     <- mdInitCond
            fpath   <- getTemp
            decodeFile fname fpath
            cont    <- forkIOCond xs $ do
                   threadDelay . ((10^6) *)
                               . floor
                               . (0.75 *) . duration $ meta
                   startDecoding cnt xs allsem (Just sem) playf
            mdWaitCondMaybe oldsem
            playf fpath meta $ do
                    putStrLn $ concat [ unpack $ artist meta
                                      , " - "
                                      , unpack $ title meta
                                      ]
                    print $ getDurationPretty $ duration meta
            mdSignalCond sem
            removeFile fpath
            case cont of
               Just _    -> return ()
               Nothing   -> mdSignalCond allsem

obtainResource :: IO (Maybe a) -> String -> IO () -> MaybeT IO a
obtainResource res str act = do
    ures <- liftIO res
    case ures of
        Nothing     -> do
                    liftIO $ do
                        putStrLn str
                        act
                    mzero

        Just val    -> return val

deinitOpenAL :: Device -> Context -> Source -> [Buffer] -> IO ()
deinitOpenAL dev ctx src bfs = do
    deleteObjectName src
    deleteObjectNames bfs
    destroyContext ctx
    void $ closeDevice dev

withOpenAL :: (Source -> [Buffer] -> IO ()) -> IO ()
withOpenAL action = void $ runMaybeT $ do
        dev <- obtainResource (openDevice Nothing)
                              "Could not open device."
                              (return ())
        ctx <- obtainResource (createContext dev [])
                              "Could not open context."
                              (void $ closeDevice dev)
        liftIO $ do
            currentContext $= Just ctx
            source      <- genObjectName            :: IO Source
            buffers     <- genObjectNames buffNum   :: IO [Buffer]
            action source buffers
            deinitOpenAL dev ctx source buffers

metaToOpenALFormat :: MDMeta -> Format
metaToOpenALFormat meta =
    case bps meta of
        8   -> case channels meta of
                1   -> Mono8
                2   -> Stereo8
        16  -> case channels meta of
                1   -> Mono16
                2   -> Stereo16

loadBuffs' :: Int -> MDData -> MDMeta -> Int -> IO MDBuffT
loadBuffs' bfn mddata meta cnt
    | cnt == 0      = return mdseq
    | otherwise     =
        hIsEOF hnd >>= \eof -> case eof of
              True    -> return mdseq
              False   -> do
                  ptr           <- callocBytes buffSize
                  buffCount     <- hGetBuf hnd ptr buffSize

                  let memReg    = MemoryRegion ptr (fromIntegral buffCount)
                  let mdNewSeq  = mdBuffAppend mdseq memReg
                  let mdNewData = MDData src bfs hnd mdNewSeq

                  bufferData (bfs !! bfn) $= BufferData memReg
                                                    (metaToOpenALFormat meta)
                                                    (fromIntegral $ sampleRate meta)

                  case buffCount < buffSize of
                      True        -> return mdNewSeq
                      False       -> return
                                 =<< loadBuffs' (bfn + 1) mdNewData meta (cnt - 1)

    where (MDData src bfs hnd mdseq) = mddata

loadBuffs :: MDData -> MDMeta -> Int -> IO MDBuffT
loadBuffs = loadBuffs' 0

playFile :: Source -> [Buffer] -> FilePath -> MDMeta -> IO () -> IO ()
playFile src bfs fpath meta onPlay = do
    handle      <- openBinaryFile fpath ReadMode
    mdBuff      <- loadBuffs (MDData src bfs handle mdBuffInit) meta buffNum
    let delay   = floor $ 10^6 * 0.5 * (getBufferDuration meta buffSize)
    onPlay
    queueBuffers src (take (mdBuffSize mdBuff) bfs)
    play [src]
    mdRemBuff   <- playLoop src handle delay meta mdBuff
    mdBuffDropAll mdRemBuff
    void $ hClose handle

checkState :: Source -> IO ()
checkState src = sourceState src >>= \st -> case st of
        Stopped     -> play [src]
        _           -> return ()

dropBuffers :: MDBuffT -> IO MDBuffT
dropBuffers mdseq =
    mdBuffDrop mdseq >>= \sq -> case sq of
        Nothing         -> return mdseq
        Just mdnewseq   -> return mdnewseq

waitUntilDone :: Source -> IO ()
waitUntilDone src = sourceState src >>= \st -> case st of
        Playing     -> waitUntilDone src
        Stopped     -> return ()
        _           -> return ()

playLoop :: Source -> Handle -> Int -> MDMeta -> MDBuffT -> IO MDBuffT
playLoop src hnd delay meta mdseq = do
    threadDelay delay
    buffersProcessed src >>= \proc -> case proc > 0 of
        True    -> do
            bfs         <- unqueueBuffers src proc
            mdNewSeq    <- loadBuffs (MDData src bfs hnd mdseq) meta (length bfs)

            let addedBuffCnt = (mdBuffSize mdNewSeq) - (mdBuffSize mdseq)
            case addedBuffCnt == 0 of
                True    -> do
                    waitUntilDone src
                    nproc <- buffersProcessed src
                    unqueueBuffers src nproc
                    return mdNewSeq

                False   -> do
                    queueBuffers src (take addedBuffCnt bfs)
                    checkState src
                    return =<< playLoop src hnd delay meta
                           =<< dropBuffers mdNewSeq

        False   -> do
            checkState src
            return =<< playLoop src hnd delay meta mdseq

main :: IO ()
main = getArgs >>= \argsr -> do
            let args = zip [0..] argsr
            withOpenAL $ \src bfs -> do
                allsem <- mdInitCond
                startDecoding (length args) args allsem Nothing
                              (playFile src bfs)
                void $ mdWaitCond allsem
