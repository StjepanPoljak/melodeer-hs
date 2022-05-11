import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.StreamDecoder
import Control.Monad.IO.Class (MonadIO (..))
import System.Environment (getArgs)
import System.IO (openTempFile, hClose)
import System.Directory (removeFile, getCurrentDirectory)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Data.Text (Text, pack)
import System.Process
import Control.Concurrent.QSem
import Control.Monad (liftM)

type Hours = Int
type Minutes = Int
type Seconds = Int

data MDDuration = MDDuration Hours Minutes Seconds deriving (Eq)

instance Show MDDuration where
    show (MDDuration 0 0 s) = show s ++ "s"
    show (MDDuration 0 m s) = show m ++ "m" ++ show (MDDuration 0 0 s)
    show (MDDuration h m s) = show h ++ "h" ++ show (MDDuration 0 m s)

type Sem = QSem

waitCond :: Sem -> IO ()
waitCond = waitQSem

signalCond :: Sem -> IO ()
signalCond = signalQSem

initCond :: IO Sem
initCond = newQSem 0

waitCondMaybe :: Maybe Sem -> IO ()
waitCondMaybe (Just sem) = waitCond sem
waitCondMaybe Nothing = return ()

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

playFile :: FilePath -> IO ()
playFile fpath = do
        (_, Just hout, _, _) <- createProcess (proc "cat" [ fpath ]) { std_out = CreatePipe }
        (_, _, _, ph) <- createProcess (proc "pacat" [] ) { std_in = UseHandle hout }
        waitForProcess ph
        return ()

forkIOCond :: IO () -> Sem -> [(Int, String)] -> IO (Maybe ThreadId)
forkIOCond _ allsem [] = do
    signalCond allsem
    return Nothing
forkIOCond cb _ (x:xs) = do
    tid <- forkIO cb
    return (Just tid)

startDecoding :: Int -> [(Int, String)] -> Sem -> Maybe Sem -> IO ()
startDecoding _ [] _ _ = return ()
startDecoding cnt (x:xs) allsem oldsem = let (no, fname) = x in do
                 printMeta fname
                 sem <- initCond
                 fpath <- getTemp
                 decodeFile fname fpath
                 forkIOCond (do
                        threadDelay . ((10^6) *)
                                    . floor
                                    . (0.75 *) =<< getDuration fname
                        startDecoding cnt xs allsem (Just sem)) allsem xs
                 waitCondMaybe oldsem
                 playFile (fpath)
                 signalCond sem
                 removeFile fpath

main :: IO ()
main = (\args -> do
            allsem <- initCond
            startDecoding (length args) args allsem Nothing
            waitCond allsem
            return ()) =<< liftM (zip [0..]) getArgs
