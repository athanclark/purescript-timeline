module Timeline.Data.TimeIndex where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Type.Proxy (Proxy)


class HumanTimeIndex a where
  -- | Printing
  humanTimeIndexToString :: a -> String
  -- | Parsing
  humanTimeIndexFromString :: String -> Either String a

class PresentableTimeIndex a where
  -- | Presenting
  presentableTimeIndexToNumber :: a -> Number

-- | Make Comparable
class Ord a <= ComparableTimeIndex a

class StorableTimeIndex a where
  -- | Storing as Json
  storableTimeIndexToJson :: a -> Json
  -- | Unpacking from Json
  storableTimeIndexFromJson :: Json -> Either String a
  -- | Storing as ArrayBuffer
  storableTimeIndexToArrayBuffer :: a -> Effect ArrayBuffer
  -- | Unpacking from ArrayBuffer
  storableTimeIndexFromArrayBuffer :: ArrayBuffer -> Effect (Maybe a)

class TimeIndex human presentable comparable storable
  | human -> storable, human -> presentable, human -> comparable, comparable -> presentable where
  humanToStorableTimeIndex :: human -> storable
  storableToHumanTimeIndex :: storable -> human
  humanToPresentableTimeIndex :: human -> presentable
  humanToComparableTimeIndex :: human -> comparable
  comparableToPresentableTimeIndex :: Proxy human -> comparable -> presentable
