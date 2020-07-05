module Timeline.UI.TimeSpan where

import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.Time.Span (DecidedSpan(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
import Data.Generic.Rep (class Generic)
import Data.Default (class Default)
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
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An inclusive span of time from `startIndex` to `stopIndex`.
-- |
-- | Defined over the user-level timescale `a`.
newtype TimeSpan
  = TimeSpan
  { name :: String
  , description :: String
  , span :: DecidedSpan
  , timeSpace :: Maybe TimeSpaceID
  , id :: UUID -- TODO trim the fat later
  }

derive instance genericTimeSpan :: Generic TimeSpan _

derive newtype instance eqTimeSpan :: Eq TimeSpan

derive newtype instance showTimeSpan :: Show TimeSpan

instance encodeJsonTimeSpan :: EncodeJson TimeSpan where
  encodeJson (TimeSpan { name, description, span, timeSpace, id }) =
    "name" := name
      ~> "description"
      := description
      ~> "span"
      := span
      ~> "timeSpace"
      := timeSpace
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeSpan :: DecodeJson TimeSpan where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    span <- o .: "span"
    let
      getUUID s = case UUID.parseUUID s of
        Nothing -> fail $ "Couldn't parse UUID: " <> s
        Just x -> pure x
    timeSpace <- o .: "timeSpace"
    id <- o .: "id" >>= getUUID
    pure $ TimeSpan { name, description, span, timeSpace, id }

instance arbitraryTimeSpan :: Arbitrary TimeSpan where
  arbitrary = do
    name <- genString
    description <- genString
    span <- arbitrary
    timeSpace <- arbitrary
    let
      id = unsafePerformEffect UUID.genUUID
    pure (TimeSpan { name, description, span, timeSpace, id })

instance defaultTimeSpan :: Default TimeSpan where
  def =
    TimeSpan
      { name: "TimeSpan"
      , description: ""
      , span: DecidedSpanNumber { start: 0.0, stop: 1.0 }
      , timeSpace: Nothing
      , id: unsafePerformEffect UUID.genUUID
      }
