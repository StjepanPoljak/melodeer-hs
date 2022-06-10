import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.StreamDecoder
import Control.Monad.IO.Class (MonadIO (..))
import System.Environment (getArgs)
import System.IO (openTempFile, hClose)
import System.Directory (removeFile, getCurrentDirectory)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.Text (Text, pack)
import System.Process
import Control.Monad (liftM)
import MDSem
import MDBuff
import System.IO (openBinaryFile, Handle, IOMode(..), hClose, hGetBuf, hIsEOF)
import Foreign.Marshal.Alloc (callocBytes)
import Sound.OpenAL

buffNum = 4
buffSize = 4096

data MDData = MDData Source [Buffer] Handle MDBuffT

type Hours = Int
type Minutes = Int
type Seconds = Int

data MDDuration = MDDuration Hours Minutes Seconds deriving (Eq)

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

getMetaString :: MetaType VorbisComment -> Text
getMetaString Nothing = pack "Unknown"
getMetaString (Just x) = x

printMeta :: String -> IO ()
printMeta fname = runFlacMeta defaultMetaSettings fname $ do
    retrieve (VorbisComment Title) >>= liftIO . print . getMetaString
    retrieve (Duration) >>= liftIO . print . getDurationPretty

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
        waitForProcess ph
        return ()

forkIOCond :: [(Int, String)] -> IO () -> IO (Maybe ThreadId)
forkIOCond [] _ = do
    return Nothing
forkIOCond _ cb = do
    tid <- forkIO cb
    return (Just tid)

startDecoding :: Int -> [(Int, String)] -> MDSemT -> Maybe MDSemT
                     -> (FilePath -> IO ()) -> IO ()
startDecoding _ [] _ _ _ = return ()
startDecoding cnt (x:xs) allsem oldsem playf = let (no, fname) = x in do
                 printMeta fname
                 sem    <- mdInitCond
                 fpath  <- getTemp
                 decodeFile fname fpath
                 cont   <- forkIOCond xs (do
                        threadDelay . ((10^6) *)
                                    . floor
                                    . (0.75 *) =<< getDuration fname
                        startDecoding cnt xs allsem (Just sem) playf)
                 mdWaitCondMaybe oldsem
                 playf fpath
                 mdSignalCond sem
                 removeFile fpath
                 case cont of
                    Just _    -> return ()
                    Nothing   -> mdSignalCond allsem

withOpenAL :: (Source -> [Buffer] -> IO ()) -> IO ()
withOpenAL action =
    (\rawDev -> case rawDev of
        Just dev    ->
            (\rawCxt -> case rawCxt of
                Nothing         -> do
                    putStrLn "Could not create context."
                    return ()

                Just context   -> do
                    currentContext $= rawCxt
                    source      <- genObjectName            :: IO Source
                    buffers     <- genObjectNames buffNum   :: IO [Buffer]
                    action source buffers
                    deleteObjectName source
                    deleteObjectNames buffers
                    destroyContext context
                    closeDevice dev
                    return ()
            ) =<< createContext dev []

        Nothing     -> do
                putStrLn "Could not create device."
                return ()
    ) =<< openDevice Nothing

loadBuffs' :: Int -> MDData -> Int -> IO MDBuffT
loadBuffs' bfn mddata cnt
    | cnt == 0      = return mdseq
    | otherwise     =
        (\eof -> case eof of
              True    -> return mdseq
              False   -> do
                  ptr           <- callocBytes buffSize
                  buffCount     <- hGetBuf hnd ptr buffSize

                  let memReg    = MemoryRegion ptr (fromIntegral buffCount)
                  let mdNewSeq  = mdBuffAppend mdseq memReg
                  let mdNewData = MDData src bfs hnd mdNewSeq

                  bufferData (bfs !! bfn) $= (BufferData memReg Stereo16 44100)

                  case buffCount < buffSize of
                      True        -> return mdNewSeq
                      False       -> return
                                 =<< loadBuffs' (bfn + 1) mdNewData (cnt - 1)
        ) =<< hIsEOF hnd

    where (MDData src bfs hnd mdseq) = mddata

loadBuffs :: MDData -> Int -> IO MDBuffT
loadBuffs = loadBuffs' 0

playFile :: Source -> [Buffer] -> FilePath -> IO ()
playFile src bfs fpath = do
    handle      <- openBinaryFile fpath ReadMode
    mdBuff      <- loadBuffs (MDData src bfs handle mdBuffInit) buffNum
    queueBuffers src (take (mdBuffSize mdBuff) bfs)
    play [src]
    mdRemBuff <- playLoop src handle mdBuff
    mdBuffDropAll mdRemBuff
    hClose handle
    return ()

checkState :: Source -> IO ()
checkState src =
    (\srcState -> case srcState of
        Stopped     -> play [src]
        _           -> return ()
    ) =<< sourceState src

dropBuffers :: MDBuffT -> IO MDBuffT
dropBuffers mdseq =
    (\dropseq -> case dropseq of
        Nothing         -> return mdseq
        Just mdnewseq   -> return mdnewseq) =<< mdBuffDrop mdseq

waitUntilDone :: Source -> IO ()
waitUntilDone src = (\srcState -> case srcState of
        Playing     -> waitUntilDone src
        Stopped     -> return ()
        _           -> return ()) =<< sourceState src

playLoop :: Source -> Handle -> MDBuffT -> IO MDBuffT
playLoop src hnd mdseq =
    (\proc -> case proc > 0 of
        True    -> do
            bfs <- unqueueBuffers src proc
            mdNewSeq <- loadBuffs (MDData src bfs hnd mdseq) (length bfs)

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
                    return =<< playLoop src hnd =<< dropBuffers mdNewSeq

        False   -> do
            checkState src
            return =<< playLoop src hnd mdseq

    ) =<< buffersProcessed src

main :: IO ()
main = (\args -> withOpenAL (\src bfs -> do
            allsem <- mdInitCond
            startDecoding (length args) args allsem Nothing (playFile src bfs)
            mdWaitCond allsem
            return ())
       ) =<< liftM (zip [0..]) getArgs
