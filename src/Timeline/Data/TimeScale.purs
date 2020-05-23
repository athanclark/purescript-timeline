module Timeline.Data.TimeScale where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (toEnumWithDefaults)
import Data.String.CodeUnits (fromCharArray)
import Data.NonEmpty (NonEmpty (..))
import Data.Tuple.Nested (type (/\), tuple3, uncurry3, tuple4, uncurry4, tuple5, uncurry5, tuple6, uncurry6)
import Data.MultiSet.Indexed (IxMultiSet)
import Data.MultiSet.Indexed (mapKeys) as IxMultiSet
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , putArrayBuffer, readArrayBuffer, byteLength)
import Data.ArrayBuffer.Class.Types (Int8 (..), Float64BE (..))
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, arrayOf, oneOf, sized, resize)
import Test.QuickCheck.UTF8String (genString)


-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index = TimeScale
  { beginIndex  :: index
  , endIndex    :: index
  -- TODO human <-> presented interpolation
  -- non-essential
  , name        :: String
  , description :: String
  , units       :: String
  , document    :: Maybe String -- TODO markdown
  }
derive instance genericTimeScale :: Generic (TimeScale index) _
derive newtype instance eqTimeScale :: Eq index => Eq (TimeScale index)
derive newtype instance ordTimeScale :: Ord index => Ord (TimeScale index)
instance functorTimeScale :: Functor TimeScale where
  map f (TimeScale x) = TimeScale x {beginIndex = f x.beginIndex, endIndex = f x.endIndex}
derive newtype instance showTimeScale :: Show index => Show (TimeScale index)
derive newtype instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index)
derive newtype instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index)
instance encodeArrayBufferTimeScale :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeScale index) where
  putArrayBuffer b o (TimeScale {beginIndex,endIndex,name,description,units,document}) =
    putArrayBuffer b o (tuple6 beginIndex endIndex name description units document)
instance decodeArrayBufferTimeScale :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeScale index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeScale index
        go = uncurry6 \beginIndex endIndex name description units document -> TimeScale {beginIndex,endIndex,name,description,units,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthTimeScale :: DynamicByteLength index => DynamicByteLength (TimeScale index) where
  byteLength (TimeScale {beginIndex,endIndex,name,description,units,document}) =
    byteLength (tuple6 beginIndex endIndex name description units document)
instance arbitraryTimeScale :: Arbitrary index => Arbitrary (TimeScale index) where
  arbitrary = do
    beginIndex <- arbitrary
    endIndex <- arbitrary
    name <- genString
    description <- genString
    units <- genString
    document <- oneOf (NonEmpty (pure Nothing) [Just <$> genString])
    pure (TimeScale {beginIndex,endIndex,name,description,units,document})

changeTimeScaleBegin :: forall index
                      . TimeScale index -> index
                     -> TimeScale index
changeTimeScaleBegin (TimeScale t) i = TimeScale t {beginIndex = i}

changeTimeScaleEnd :: forall index
                    . TimeScale index -> index
                   -> TimeScale index
changeTimeScaleEnd (TimeScale t) i = TimeScale t {endIndex = i}

