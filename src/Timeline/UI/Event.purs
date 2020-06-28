module Timeline.UI.Event where

import Timeline.Time.Value (DecidedValue(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  , fail
  )
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
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
  , id :: UUID
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
      := UUID.toString id
      ~> "time"
      := time
      ~> jsonEmptyObject

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    id' <- o .: "id"
    time <- o .: "time"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure (Event { name, description, id, time })

instance arbitraryEvent :: Arbitrary Event where
  arbitrary = do
    name <- genString
    description <- genString
    let
      id = unsafePerformEffect UUID.genUUID
    time <- arbitrary
    pure (Event { name, description, id, time })

instance defaultEvent :: Default Event where
  def =
    Event
      { name: "Event"
      , description: ""
      , id: unsafePerformEffect UUID.genUUID
      , time: DecidedValueNumber 0.0
      }
