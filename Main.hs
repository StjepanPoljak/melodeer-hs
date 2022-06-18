import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.StreamDecoder
import Control.Monad.IO.Class (MonadIO (..))
import System.Environment (getArgs)
import System.IO (openTempFile, hClose)
import System.Directory (removeFile, getCurrentDirectory)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.Text (Text, pack, unpack)
import System.Process
import Control.Monad (liftM, void, mzero, unless, when, join, mapM_, guard)
import MDSem
import MDBuff
import System.IO (openBinaryFile, Handle, IOMode(..), hClose, hGetBuf, hIsEOF)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import Foreign.Marshal.Alloc (callocBytes)
import Sound.OpenAL
import qualified Data.Map as Map (fromList, (!?), (!))
import Data.Maybe (isNothing, fromJust)
import Control.Exception (try, SomeException(..), evaluate)

data MDData = MDData { source       :: Source
                     , buffers      :: [Buffer]
                     , handle       :: Handle
                     , mdbuffer     :: MDBuffT
                     }

type PlayFileT = FilePath -> MDMeta -> IO () -> ReaderT MDOpts IO ()

data MDDriver = MDPacat | MDOpenAL deriving (Eq)

data MDDelay = MDDelay { coeff      :: Double
                       , offset     :: Double
                       } deriving (Eq, Show)

data MDOpts = MDOpts { buffNum      :: Int
                     , buffSize     :: Int
                     , format       :: String
                     , playlist     :: [(Int, FilePath)]
                     , plistLen     :: Int
                     , allsem       :: MDSemT
                     , driver       :: MDDriver
                     , delay        :: MDDelay
                     , decDelay     :: Double
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

metaList = [ ('c', ( (\meta _ _ -> show $ channels meta)
             , "channels")
             )
           , ('b', ( (\meta _ _ -> show $ bps meta)
             , "bits per sample")
             )
           , ('s', ( (\meta _ _ -> show $ sampleRate meta)
             , "sample rate")
             )
           , ('d', ( (\meta _ _ -> show $ getDurationPretty $ duration meta)
             , "duration")
             )
           , ('t', ( (\meta _ _ -> unpack $ title meta)
             , "song title")
             )
           , ('a', ( (\meta _ _ -> unpack $ artist meta)
             , "song artist")
             )
           , ('f', ( (\_ (_, fn) _ -> show $ fn)
             , "file name")
             )
           , ('n', ( (\_ (no, _) _ -> show $ no)
             , "playlist track number")
             )
           , ('N', ( (\_ _ totl -> show $ totl)
             , "total number of playlist items")
             )
           ]

metaMap = Map.fromList metaList

formatMeta :: String -> MDMeta -> (Int, FilePath) -> Int -> String
formatMeta part meta (no, fname) plen
    | length part < 2   = part
    | otherwise         = case head part of
                '%'     -> let part'    = drop 2 part
                               fmt      = take 2 part
                               newstr   = mapMeta fmt meta (no, fname) plen
                           in (maybe fmt id newstr) ++ (formatMeta part' meta
                                                                   (no, fname)
                                                                   plen)

                _       -> let (x:xs)   = part
                           in x:(formatMeta xs meta (no, fname) plen)

    where mapMeta :: String -> MDMeta -> (Int, FilePath) -> Int -> Maybe String
          mapMeta str mt nf pl = liftM (\val -> (fst val) mt nf pl)
                               $ metaMap Map.!? (str !! 1)

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

playFilePacat :: FilePath -> MDMeta -> IO () -> ReaderT MDOpts IO ()
playFilePacat fpath _ onPlay = liftIO $ do
        onPlay
        (_, Just hout, _, _) <- createProcess (proc "cat" [ fpath ]) { std_out = CreatePipe }
        (_, _, _, ph) <- createProcess (proc "pacat" [] ) { std_in = UseHandle hout }
        void $ waitForProcess ph

startDecoding' :: [(Int, FilePath)] -> Maybe MDSemT -> PlayFileT
               -> ReaderT MDOpts IO ()
startDecoding' [] oldsem _ = return ()
startDecoding' ((no, fname):xs) oldsem pf =
    ask >>= \opts -> liftIO $ do
        meta    <- getMetadata fname
        sem     <- mdInitCond
        fpath   <- getTemp
        decodeFile fname fpath
        unless (playlistEnd) $ void $ forkIO $ do
                        threadDelay . ((10^6) *)
                                    . floor
                                    . ((decDelay opts) *)
                                    . duration $ meta
                        runReaderT (startDecoding' xs (Just sem) pf)
                                   opts

        mdWaitCondMaybe oldsem

        (flip runReaderT) opts $ pf fpath meta $ putStrLn
                                               . formatMeta (format opts)
                                                            meta (no, fname)
                                               $ plistLen opts
        mdSignalCond sem
        removeFile fpath
        when playlistEnd $ mdSignalCond (allsem opts)

    where playlistEnd = length xs == 0

startDecoding :: PlayFileT -> ReaderT MDOpts IO ()
startDecoding pf = ask >>= \opts -> startDecoding' (playlist opts) Nothing pf

obtainResource :: IO (Maybe a) -> String -> IO () -> MaybeT IO a
obtainResource res str act = liftIO res >>= maybe act' return
    where act' = do
            liftIO $ do
                when (length str > 0) $ putStrLn str
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

metaToOpenALFormat :: MDMeta -> Maybe Format
metaToOpenALFormat meta = Map.fromList [ (( 8, 1), Mono8)
                                       , (( 8, 2), Stereo8)
                                       , ((16, 1), Mono16)
                                       , ((16, 2), Stereo16)
                                       ] Map.!? (bps meta, channels meta)

loadBuffs' :: Int -> MDData -> Int -> Format -> Int -> Int -> IO MDBuffT
loadBuffs' bfn mddata rate format cnt bfsize
    | cnt == 0      = return (mdbuffer mddata)
    | otherwise     =
    hIsEOF (handle mddata) >>= \eof -> case eof of

        True    -> return (mdbuffer mddata)
        False   -> do
           ptr           <- callocBytes bfsize
           buffCount     <- hGetBuf (handle mddata) ptr bfsize

           let memReg     = MemoryRegion ptr (fromIntegral buffCount)
           let mdNewSeq   = mdBuffAppend (mdbuffer mddata) memReg

           bufferData ((buffers mddata) !! bfn) $= BufferData memReg format
                                                   (fromIntegral $ rate)

           case buffCount < bfsize of
              True        -> return mdNewSeq
              False       -> return
                         =<< loadBuffs' (bfn + 1)
                                        (mddata { mdbuffer = mdNewSeq })
                                        rate format (cnt - 1) bfsize

loadBuffs :: MDData -> Int -> Format -> Int -> Int -> IO MDBuffT
loadBuffs = loadBuffs' 0

playFile :: Source -> [Buffer] -> FilePath -> MDMeta -> IO ()
         -> ReaderT MDOpts IO ()
playFile src bfs fpath meta onPlay
    | isNothing fmt'    = liftIO $ putStrLn "Format not supported."
    | otherwise         = ask >>= \opts -> liftIO $ do
        let fmt  = fromJust fmt'
        hnd     <- openBinaryFile fpath ReadMode
        let md   = MDData src bfs hnd mdBuffInit
        mdbfs   <- loadBuffs md (sampleRate meta) fmt (buffNum opts)
                             (buffSize opts)
        let dl'  = (fromIntegral $ buffNum opts) * (coeff $ delay opts)
                 + (offset $ delay opts)
        let dl   = floor $ 10^6 * dl' * (getBufferDuration meta (buffSize opts))

        onPlay
        queueBuffers src (take (mdBuffSize mdbfs) bfs)
        play [src]
        mdBuffDropAll =<< playLoop src hnd dl (sampleRate meta) fmt
                                   (buffSize opts) mdbfs
        void $ hClose hnd

    where fmt' = metaToOpenALFormat meta

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

playLoop :: Source -> Handle -> Int -> Int -> Format -> Int -> MDBuffT -> IO MDBuffT
playLoop src hnd delay rate format bfsize mdseq = do
    threadDelay delay
    buffersProcessed src >>= \proc -> case proc > 0 of
        True    -> do
            bfs         <- unqueueBuffers src proc
            let md       = MDData src bfs hnd mdseq
            mdNewSeq    <- loadBuffs md rate format (length bfs) bfsize

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
                    return =<< playLoop src hnd delay rate format bfsize
                           =<< dropBuffers mdNewSeq

        False   -> do
            checkState src
            return =<< playLoop src hnd delay rate format bfsize mdseq

instance Read MDDelay where
    readsPrec _ str = maybe [] (\d -> [(d, "")]) (getDelayCoeff str)

getDelayCoeff' :: (String, String) -> Char -> String -> Maybe MDDelay
getDelayCoeff' (a, b) 'b' [] = Just $ MDDelay (read a :: Double) (read b :: Double)
getDelayCoeff' (a, b) ch (x:xs)
    | ch == 'a'     = case x of
        ':'     -> getDelayCoeff' (a, b) 'b' xs
        _       -> getDelayCoeff' (a ++ [x], b) ch xs
    | ch == 'b'     = getDelayCoeff' (a, b ++ [x]) ch xs
getDelayCoeff' _ _ _ = Nothing

getDelayCoeff = getDelayCoeff' ("", "") 'a'

readsMaybe x = case reads x of
        [(res, [])]     -> Just res
        _               -> Nothing

optList = [ ('n', ( (\opts x -> liftM (\res -> opts { buffNum = res } )
                              $ readsMaybe x )
            , "number of buffers")
            )
          , ('s', ( (\opts x -> liftM (\res -> opts { buffSize = res } )
                              $ readsMaybe x )
            , "buffer size")
            )
          , ('f', ( (\opts x -> Just $ opts { format = x } )
            , "text format")
            )
          , ('l', ( idOpts
            , "open playlist file")
            )
          , ('D', ( (\opts x -> liftM (\drv -> opts { driver = drv } )
                              $ Map.fromList [ ("pacat", MDPacat)
                                             , ("openal", MDOpenAL)
                                             ] Map.!? x)
            , "driver (pacat, openal)")
            )
          , ('t', ( (\opts x -> liftM (\res -> opts { delay = res } )
                              $ readsMaybe x)
            , "loop delay (a * buffNum + b)")
            )
          , ('d', ( (\opts x -> liftM (\res -> opts { decDelay = res } )
                              $ readsMaybe x)
            , "decoder delay (between 0 and 1)")
            )
          , ('h', ( idOpts
            , "show help")
            )
          ]
    where idOpts opts _ = Just opts

showHelpFor prefix = map (\(ch, (_, help)) -> "\t" ++ (prefix:ch:"\t") ++ help)

showHelp = mapM_ putStrLn $ concat
    [
        [ "melodeer-hs is a simple FLAC player written in"
        , "Haskell using OpenAL."
        , ""
        , "melodeer-hs <file_list> [-l <playlist_file>]"
        , "\t\t[-D pacat|openal] [-f <text_format>]"
        , "\t\t[-n <buff_num>] [-s <buff_size>] [-h]"
        , "\t\t[-t <loop_delay>] [-d <decoder_delay>]"
        , ""
        , "Options:"
        , ""
        ], showHelpFor '-' optList,
        [ ""
        , "Loop delay is of format a:b (e.g. 0.0:0.5, which"
        , "amounts to (0.0 * buffNum + 0.5) multiplied by"
        , "single buffer time."
        , ""
        , "Text format can take following fields:"
        , ""
        ], showHelpFor '%' metaList
    ]

optMap = Map.fromList optList

getOpts' :: Maybe Char -> MDOpts -> [String] -> IO (Maybe MDOpts)
getOpts' Nothing part [] = return $ Just part
getOpts' _ _ [] = return Nothing
getOpts' Nothing part (x:xs)
    | head x /= '-' || length x /= 2    = do
                                    putStrLn $ "Invalid command line usage."
                                            ++ "Try -h for help."
                                    return Nothing
    | otherwise                         = return
                                        =<< getOpts' (Just (x !! 1)) part xs
getOpts' (Just ch) opts (x:xs) = case ch of
    'l'     -> return =<< (\pl ->
        let newpl = zip [1..] ((snd . unzip . playlist $ opts) ++ pl)
        in getOpts' Nothing (opts { playlist = newpl
                                  , plistLen = length newpl
                                  }) xs) =<< liftM lines (readFile x)
    'h'     -> do
            showHelp
            return Nothing

    cmd     -> maybe (do
                        putStrLn $ "Invalid option: \"" ++ '-':[cmd]
                                ++ "\". Try -h for help."
                        return Nothing)
                     (\val -> case (fst val) opts x of
                            Nothing     -> do
                                putStrLn $ "Invalid argument \"" ++ x
                                        ++ "\" for option \"" ++ '-':[cmd]
                                        ++ "\". Try -h for help."
                                return Nothing
                            Just opts'  -> getOpts' Nothing opts' xs
                     ) (optMap Map.!? ch)

getOpts :: MDOpts -> [String] -> IO (Maybe MDOpts)
getOpts = getOpts' Nothing

main :: IO ()
main = getArgs >>= \args -> do
            let pl       = zip [1..]
                         . takeWhile (\x -> head x /= '-')
                         $ args
            let pllen    = length pl
            let opts'    = drop pllen args

            allsem      <- mdInitCond
            let defOpts  = MDOpts 4 4096 "(%n/%N) %a - %t (%d)" pl pllen
                                 allsem MDOpenAL (MDDelay 0.0 0.75) 0.75

            void $ liftIO $ runMaybeT $ do

                opts    <- obtainResource (getOpts defOpts opts') ""
                                          (return ())

                unless (plistLen opts /= 0) $ liftIO
                                            $ putStrLn $ "No files added to "
                                           ++ "playlist. Try -h for help."
                guard (plistLen opts /= 0)

                void $ liftIO $ (flip runReaderT) opts $ case driver opts of

                    MDOpenAL    -> withOpenAL $ \src bfs -> do
                                        startDecoding (playFile src bfs)
                                        liftIO $ mdWaitCond allsem

                    MDPacat     -> do
                            startDecoding playFilePacat
                            liftIO $ mdWaitCond allsem

