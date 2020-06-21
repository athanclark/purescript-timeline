module Timeline.Data.Event where

import Prelude
import Data.Maybe (Maybe(..))
import Data.UInt (fromInt, toInt) as UInt
import Data.Tuple.Nested (type (/\), tuple4, uncurry4)
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
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | A punctuated event in time
newtype Event
  = Event
  { name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  , id :: UUID
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance ordEvent :: Ord Event

derive newtype instance showEvent :: Show Event

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event { name, description, document, id }) =
    "name" := name
      ~> "description"
      := description
      ~> "document"
      := document
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    document <- o .: "document"
    id' <- o .: "id"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure $ Event { name, description, document, id }

instance encodeArrayBufferEvent :: EncodeArrayBuffer Event where
  putArrayBuffer b o (Event { name, description, document, id }) = putArrayBuffer b o (tuple4 name description document fixedBytesId)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance decodeArrayBufferEvent :: DecodeArrayBuffer Event where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe Event)
      go =
        uncurry4 \name description document id' -> case UUID.parseBytesUUID (getBytes id') of
          Nothing -> throw $ "Couldn't parse UUID: " <> show id'
          Just id -> pure $ Just $ Event { name, description, document, id }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs
    where
    getBytes = map (\(Uint8 x) -> UInt.toInt x)

instance dynamicByteLengthEvent :: DynamicByteLength Event where
  byteLength (Event { name, description, document, id }) = byteLength (tuple4 name description document fixedBytesId)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    document <- genString
    let
      id = unsafePerformEffect (UUID.genUUID)
    pure (Event { name, description, document, id })
