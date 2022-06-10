module MDBuff
    (
        MDBuffT(..),
        MDBuffElT(..),
        mdBuffInit,
        mdBuffGet,
        mdBuffDrop,
        mdBuffDropAll,
        mdBuffAppend,
        mdBuffSize
    ) where

import Sound.OpenAL
import Data.Sequence
import Data.Word
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr)

type MDBuffElT = MemoryRegion Word8
type MDBuffT = Seq MDBuffElT

mdBuffInit :: MDBuffT
mdBuffInit = empty

mdBuffGet :: MDBuffT -> Maybe (MDBuffElT, MDBuffT)
mdBuffGet buff = case viewl buff of
    EmptyL      -> Nothing
    mr :< buff' -> Just (mr, buff')

mdBuffDrop :: MDBuffT -> IO (Maybe (MDBuffT))
mdBuffDrop seq = do
    case mdBuffGet seq of
        Nothing                             -> return Nothing
        Just ((MemoryRegion ptr _), seq')   -> do
                            free ptr
                            return (Just seq')

mdBuffDropAll :: MDBuffT -> IO ()
mdBuffDropAll seq =
    (\useq -> case useq of
        Nothing     -> return ()
        Just seq'   -> mdBuffDropAll seq'
    ) =<< mdBuffDrop seq

mdBuffAppend :: MDBuffT -> MemoryRegion Word8 -> MDBuffT
mdBuffAppend buff mr = buff |> mr

mdBuffSize :: MDBuffT -> Int
mdBuffSize = Data.Sequence.length
