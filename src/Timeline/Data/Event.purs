module Timeline.Data.Event where

import Prelude
import Data.Maybe (Maybe(..))
import Data.UInt (fromInt, toInt) as UInt
import Data.Tuple.Nested (type (/\), tuple5, uncurry5)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson, fail)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Uint8(..))
import Data.UUID (UUID)
import Data.UUID (toBytes, toString, parseUUID, parseBytesUUID, genUUID) as UUID
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | A punctuated event in time
newtype Event index
  = Event
  { name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  , id :: UUID
  , time :: index
  }

derive instance genericEvent :: Generic index index' => Generic (Event index) _

derive newtype instance eqEvent :: Eq index => Eq (Event index)

derive newtype instance ordEvent :: Ord index => Ord (Event index) -- FIXME sort by time field first?

derive newtype instance showEvent :: Show index => Show (Event index)

instance functorEvent :: Functor Event where
  map f (Event x) = Event x { time = f x.time }

instance encodeJsonEvent :: EncodeJson index => EncodeJson (Event index) where
  encodeJson (Event { name, description, document, id, time }) =
    "name" := name
      ~> "description"
      := description
      ~> "document"
      := document
      ~> "id"
      := UUID.toString id
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson index => DecodeJson (Event index) where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    document <- o .: "document"
    id' <- o .: "id"
    time <- o .: "time"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure $ Event { name, description, document, id, time }

instance encodeArrayBufferEvent :: EncodeArrayBuffer index => EncodeArrayBuffer (Event index) where
  putArrayBuffer b o (Event { name, description, document, id, time }) = putArrayBuffer b o (tuple5 name description document fixedBytesId time)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance decodeArrayBufferEvent :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (Event index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe (Event index))
      go =
        uncurry5 \name description document id' time -> case UUID.parseBytesUUID (getBytes id') of
          Nothing -> throw $ "Couldn't parse UUID: " <> show id'
          Just id -> pure $ Just $ Event { name, description, document, id, time }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs
    where
    getBytes = map (\(Uint8 x) -> UInt.toInt x)

instance dynamicByteLengthEvent :: DynamicByteLength index => DynamicByteLength (Event index) where
  byteLength (Event { name, description, document, id, time }) = byteLength (tuple5 name description document fixedBytesId time)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance arbitraryEvent :: Arbitrary index => Arbitrary (Event index) where
  arbitrary = do
    name <- genString
    description <- genString
    document <- genString
    let
      id = unsafePerformEffect (UUID.genUUID)
    time <- arbitrary
    pure (Event { name, description, document, id, time })
