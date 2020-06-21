module Timeline.Data.TimeScale where

import Prelude
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple.Nested (type (/\), tuple6, uncurry6)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson)
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

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index
  = TimeScale
  { beginIndex :: index
  , endIndex :: index
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
  map f (TimeScale x) = TimeScale x { beginIndex = f x.beginIndex, endIndex = f x.endIndex }

instance showTimeScale :: (Show index) => Show (TimeScale index) where
  show = genericShow

derive newtype instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index)

derive newtype instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index)

instance encodeArrayBufferTimeScale :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeScale index) where
  putArrayBuffer b o (TimeScale { beginIndex, endIndex, name, description, units, document }) = putArrayBuffer b o (tuple6 beginIndex endIndex name description units document)

instance decodeArrayBufferTimeScale :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeScale index) where
  readArrayBuffer b o =
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeScale index
      go = uncurry6 \beginIndex endIndex name description units document -> TimeScale { beginIndex, endIndex, name, description, units, document }
    in
      map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeScale :: DynamicByteLength index => DynamicByteLength (TimeScale index) where
  byteLength (TimeScale { beginIndex, endIndex, name, description, units, document }) = byteLength (tuple6 beginIndex endIndex name description units document)

instance arbitraryTimeScale :: Arbitrary index => Arbitrary (TimeScale index) where
  arbitrary = do
    beginIndex <- arbitrary
    endIndex <- arbitrary
    name <- genString
    description <- genString
    units <- genString
    document <- oneOf (NonEmpty (pure Nothing) [ Just <$> genString ])
    pure (TimeScale { beginIndex, endIndex, name, description, units, document })

changeTimeScaleBegin ::
  forall index.
  TimeScale index ->
  index ->
  TimeScale index
changeTimeScaleBegin (TimeScale t) i = TimeScale t { beginIndex = i }

changeTimeScaleEnd ::
  forall index.
  TimeScale index ->
  index ->
  TimeScale index
changeTimeScaleEnd (TimeScale t) i = TimeScale t { endIndex = i }
