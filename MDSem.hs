module MDSem
    (
        MDSemT,
        mdWaitCond,
        mdWaitCondMaybe,
        mdSignalCond,
        mdInitCond
    ) where

import Control.Concurrent.QSem

type MDSemT = QSem

mdWaitCond :: MDSemT -> IO ()
mdWaitCond = waitQSem

mdSignalCond :: MDSemT -> IO ()
mdSignalCond = signalQSem

mdInitCond :: IO MDSemT
mdInitCond = newQSem 0

mdWaitCondMaybe :: Maybe MDSemT -> IO ()
mdWaitCondMaybe (Just sem) = mdWaitCond sem
mdWaitCondMaybe Nothing = return ()


