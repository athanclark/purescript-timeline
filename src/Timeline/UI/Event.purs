module Timeline.UI.Event where

import Timeline.Time.Value (DecidedValue(..))
import Timeline.ID.Event (EventID(..))
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
import Data.UUID (genUUID) as UUID
import Data.Default (class Default)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An event documented at time `index`.
-- |
-- | Defined over the user-level timescale `a`.
newtype Event
  = Event
  { name :: String
  , description :: String
  , id :: EventID
  , time :: DecidedValue
  }

derive instance genericEvent :: Generic Event _

derive newtype instance eqEvent :: Eq Event

derive newtype instance showEvent :: Show Event

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson (Event { name, description, id, time }) =
    "name" := name
      ~> "description"
      := description
      ~> "id"
      := id
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    id <- o .: "id"
    time <- o .: "time"
    pure (Event { name, description, id, time })

instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    id <- arbitrary
    time <- arbitrary
    pure (Event { name, description, id, time })

instance defaultEvent :: Default Event where
  def =
    Event
      { name: "Event"
      , description: ""
      , id: EventID (unsafePerformEffect UUID.genUUID)
      , time: DecidedValueNumber 0.0
      }
