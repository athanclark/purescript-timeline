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



-- | Class that defines how this index is presented to humans, and how they can enter them in manually.
class HumanTimeIndex human interface | human -> interface where
  -- | Presenting the human form as an abbreviated string, for bubbles
  humanTimeIndexToString :: human -> String
  -- | Presenting the human form as an interface type for detailed adjustment
  humanTimeIndexToInterface :: human -> interface
  -- | Interpreting the human interfacing form
  humanTimeIndexFromInterface :: interface -> Either String human

-- FIXME what about integer positions, in some view box that's also an integer?
-- | How time-indexed data will be positioned spatially in a `TimeSpace`, according to some `TimeScale`.
class PositionedTimeIndex positioned where
  -- | Presenting
  positionedTimeIndexToNumber :: positioned -> Number

-- | Methods that time-indexed data will be stored; either in JSON or as binary.
class StorableTimeIndex storable where
  -- | Storing as Json
  storableTimeIndexEncodeJson :: storable -> Json
  -- | Unpacking from Json
  storableTimeIndexDecodeJson :: Json -> Either String storable
  -- | Storing as ArrayBuffer
  storableTimeIndexPutArrayBuffer :: ArrayBuffer -> ByteOffset -> storable -> Effect (Maybe ByteLength)
  -- | Unpacking from ArrayBuffer
  storableTimeIndexReadArrayBuffer :: ArrayBuffer -> ByteOffset -> Effect (Maybe storable)

-- | A complete suite of time indicies, oriented around its most human and lossless form.
class ( HumanTimeIndex human interface
      , PositionedTimeIndex positioned
      , StorableTimeIndex storable
      ) <= TimeIndex human interface positioned storable
  | human -> interface, human -> storable, human -> positioned where
  humanToStorableTimeIndex :: human -> storable
  storableToHumanTimeIndex :: storable -> human
  humanToPresentableTimeIndex :: human -> positioned


-- ------------------ Instances

-- --------- Number

instance humanTimeIndexNumber :: HumanTimeIndex Number String where
  humanTimeIndexToString = show
  humanTimeIndexToInterface = show
  humanTimeIndexFromInterface s =
    case Num.fromString s of
      Nothing -> Left $ "Not a valid number string: " <> show s
      Just x -> Right x
instance positionedTimeIndexNumber :: PositionedTimeIndex Number where
  positionedTimeIndexToNumber = identity
instance storableTimeIndexNumber :: StorableTimeIndex Number where
  storableTimeIndexEncodeJson = encodeJson
  storableTimeIndexDecodeJson = decodeJson
  storableTimeIndexPutArrayBuffer b o x = putArrayBuffer b o (Float64BE x)
  storableTimeIndexReadArrayBuffer b o = (map (\(Float64BE x) -> x)) <$> readArrayBuffer b o
instance timeIndexNumber :: TimeIndex Number String Number Number where
  humanToStorableTimeIndex = identity
  storableToHumanTimeIndex = identity
  humanToPresentableTimeIndex = identity
