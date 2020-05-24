module Timeline.Data.Event where

import Prelude
import Data.Tuple.Nested (type (/\), tuple3, uncurry3)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , putArrayBuffer, readArrayBuffer, byteLength)
import Test.QuickCheck (class Arbitrary)
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
