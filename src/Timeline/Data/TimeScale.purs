module Timeline.Data.TimeScale where

import Timeline.Time.Bounds
  ( Bounds
  , encodeJsonBounds
  , decodeJsonBounds
  , putArrayBufferBounds
  , readArrayBufferBounds
  , byteLengthBounds
  , genBounds
  )
import Prelude
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple.Nested (type (/\), tuple4, uncurry4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), fail, decodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Test.QuickCheck.UTF8String (genString)
import Type.Proxy (Proxy(..))

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index
  = TimeScale
  { bounds :: Bounds index
  -- TODO human <-> presented interpolation
  -- non-essential
  , name :: String
  , description :: String
  , units :: String
  , document :: Maybe String -- TODO markdown
  }

derive instance genericTimeScale :: Generic (TimeScale index) _

derive newtype instance eqTimeScale :: Eq index => Eq (TimeScale index)

derive newtype instance ordTimeScale :: Ord index => Ord (TimeScale index)

instance functorTimeScale :: Functor TimeScale where
  map f (TimeScale x) = TimeScale x { bounds = { begin: f x.bounds.begin, end: f x.bounds.end } }

instance showTimeScale :: Show index => Show (TimeScale index) where
  show = genericShow

-- derive newtype instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index)
instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index) where
  encodeJson (TimeScale { bounds, name, description, units, document }) =
    "bounds" := encodeJsonBounds (Proxy :: Proxy index) bounds
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "units"
      := units
      ~> "document"
      := document
      ~> jsonEmptyObject

-- derive newtype instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index)
instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index) where
  decodeJson json = do
    o <- decodeJson json
    bounds <- o .: "bounds" >>= decodeJsonBounds (Proxy :: Proxy index)
    name <- o .: "name"
    description <- o .: "description"
    units <- o .: "units"
    document <- o .: "document"
    pure (TimeScale { bounds, name, description, units, document })

instance encodeArrayBufferTimeScale :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeScale index) where
  putArrayBuffer b o (TimeScale { bounds, name, description, units, document }) = do
    mW <- putArrayBufferBounds (Proxy :: Proxy index) b o bounds
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBuffer b (o + w) (tuple4 name description units document)
        case mW' of
          Nothing -> pure (Just w)
          Just w' -> pure (Just (w + w'))

instance decodeArrayBufferTimeScale :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeScale index) where
  readArrayBuffer b o = do
    mBounds <- readArrayBufferBounds (Proxy :: Proxy index) b o
    case mBounds of
      Nothing -> pure Nothing
      Just bounds -> do
        l <- byteLengthBounds (Proxy :: Proxy index) bounds
        let
          go :: _ /\ _ /\ _ /\ _ /\ Unit -> TimeScale index
          go = uncurry4 \name description units document -> TimeScale { bounds, name, description, units, document }
        map go <$> readArrayBuffer b (o + l)

instance dynamicByteLengthTimeScale :: DynamicByteLength index => DynamicByteLength (TimeScale index) where
  byteLength (TimeScale { bounds, name, description, units, document }) = do
    l <- byteLengthBounds (Proxy :: Proxy index) bounds
    l' <- byteLength (tuple4 name description units document)
    pure (l + l')

instance arbitraryTimeScale :: Arbitrary index => Arbitrary (TimeScale index) where
  arbitrary = do
    bounds <- genBounds (Proxy :: Proxy index)
    name <- genString
    description <- genString
    units <- genString
    document <- oneOf (NonEmpty (pure Nothing) [ Just <$> genString ])
    pure (TimeScale { bounds, name, description, units, document })
