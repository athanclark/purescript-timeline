module Timeline.Data.TimeIndex where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (Json, encodeJson, decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.ArrayBuffer.Class (putArrayBuffer, readArrayBuffer)
import Data.ArrayBuffer.Class.Types (Float64BE (..))
import Data.Number (fromString) as Num
import Effect (Effect)
import Type.Proxy (Proxy (..))


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
  storableTimeIndexEncodeJson :: a -> Json
  -- | Unpacking from Json
  storableTimeIndexDecodeJson :: Json -> Either String a
  -- | Storing as ArrayBuffer
  storableTimeIndexPutArrayBuffer :: ArrayBuffer -> ByteOffset -> a -> Effect (Maybe ByteLength)
  -- | Unpacking from ArrayBuffer
  storableTimeIndexReadArrayBuffer :: ArrayBuffer -> ByteOffset -> Effect (Maybe a)

class TimeIndex human presentable comparable storable
  | human -> storable, human -> presentable, human -> comparable, comparable -> presentable where
  humanToStorableTimeIndex :: human -> storable
  storableToHumanTimeIndex :: storable -> human
  humanToPresentableTimeIndex :: human -> presentable
  humanToComparableTimeIndex :: human -> comparable
  comparableToPresentableTimeIndex :: Proxy human -> comparable -> presentable


-- ------------------ Instances

-- --------- Number

instance humanTimeIndexNumber :: HumanTimeIndex Number where
  humanTimeIndexToString = show
  humanTimeIndexFromString s =
    case Num.fromString s of
      Nothing -> Left $ "Not a valid number string: " <> show s
      Just x -> Right x
instance presentableTimeIndexNumber :: PresentableTimeIndex Number where
  presentableTimeIndexToNumber = identity
instance comparableTimeIndexNumber :: ComparableTimeIndex Number
instance storableTimeIndexNumber :: StorableTimeIndex Number where
  storableTimeIndexEncodeJson = encodeJson
  storableTimeIndexDecodeJson = decodeJson
  storableTimeIndexPutArrayBuffer b o x = putArrayBuffer b o (Float64BE x)
  storableTimeIndexReadArrayBuffer b o = (map (\(Float64BE x) -> x)) <$> readArrayBuffer b o
instance timeIndexNumber :: TimeIndex Number Number Number Number where
  humanToStorableTimeIndex = identity
  storableToHumanTimeIndex = identity
  humanToPresentableTimeIndex = identity
  humanToComparableTimeIndex = identity
  comparableToPresentableTimeIndex Proxy = identity
