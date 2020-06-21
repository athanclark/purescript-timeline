module Timeline.UI.Event where

import Timeline.UI.Index.Value (DecidedValue)
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An event documented at time `index`.
-- |
-- | Defined over the user-level timescale `a`.
newtype Event
  = Event
  { name :: String
  , description :: String
  , time :: DecidedValue
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance showEvent :: Show Event

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event { name, description, time }) =
    "name" := name
      ~> "description"
      := description
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    time <- o .: "time"
    pure (Event { name, description, time })

instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    time <- arbitrary
    pure (Event { name, description, time })
