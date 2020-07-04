module Timeline.Data.Event where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
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
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | A punctuated event in time
newtype Event index
  = Event
  { name :: String
  , description :: String
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

instance foldableEvent :: Foldable Event where
  foldr f acc (Event x) = f x.time acc
  foldl f acc (Event x) = f acc x.time
  foldMap f (Event x) = f x.time

instance traversableEvent :: Traversable Event where
  traverse f (Event x) = (\y -> Event x { time = y }) <$> f x.time
  sequence (Event x) = (\y -> Event x { time = y }) <$> x.time

instance encodeJsonEvent :: EncodeJson index => EncodeJson (Event index) where
  encodeJson (Event { name, description, id, time }) =
    "name" := name
      ~> "description"
      := description
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
    id' <- o .: "id"
    time <- o .: "time"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure $ Event { name, description, id, time }

instance encodeArrayBufferEvent :: EncodeArrayBuffer index => EncodeArrayBuffer (Event index) where
  putArrayBuffer b o (Event { name, description, id, time }) = putArrayBuffer b o (tuple4 name description fixedBytesId time)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance decodeArrayBufferEvent :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (Event index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe (Event index))
      go =
        uncurry4 \name description id' time -> case UUID.parseBytesUUID (getBytes id') of
          Nothing -> throw $ "Couldn't parse UUID: " <> show id'
          Just id -> pure $ Just $ Event { name, description, id, time }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs
    where
    getBytes = map (\(Uint8 x) -> UInt.toInt x)

instance dynamicByteLengthEvent :: DynamicByteLength index => DynamicByteLength (Event index) where
  byteLength (Event { name, description, id, time }) = byteLength (tuple4 name description fixedBytesId time)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance arbitraryEvent :: Arbitrary index => Arbitrary (Event index) where
  arbitrary = do
    name <- genString
    description <- genString
    let
      id = unsafePerformEffect (UUID.genUUID)
    time <- arbitrary
    pure (Event { name, description, id, time })
