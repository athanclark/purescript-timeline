module Timeline.Data.Event where

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


-- | A punctuated event in time
newtype Event = Event
  { name        :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO color
  }
derive instance genericEvent :: Generic Event _
derive newtype instance eqEvent :: Eq Event
derive newtype instance ordEvent :: Ord Event
derive newtype instance showEvent :: Show Event
derive newtype instance encodeJsonEvent :: EncodeJson Event
derive newtype instance decodeJsonEvent :: DecodeJson Event
instance encodeArrayBufferEvent :: EncodeArrayBuffer Event where
  putArrayBuffer b o (Event {name,description,document}) =
    putArrayBuffer b o (tuple3 name description document)
instance decodeArrayBufferEvent :: DecodeArrayBuffer Event where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ Unit -> Event
        go = uncurry3 \name description document ->
          Event {name,description,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthEvent :: DynamicByteLength Event where
  byteLength (Event {name,description,document}) =
    byteLength (tuple3 name description document)
instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    document <- genString
    pure (Event {name,description,document})
