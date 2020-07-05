module Timeline.UI.TimeSpace.TimeScale where

import Timeline.Time.MaybeLimit
  ( DecidedMaybeLimit(DecidedMaybeLimitNumber)
  , MaybeLimit(NothingLimit)
  )
import Prelude
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (.:)
  , (~>)
  , jsonEmptyObject
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

newtype TimeScale
  = TimeScale
  { name :: String
  , units :: String
  , description :: String
  , limit :: DecidedMaybeLimit -- Commands the consistency throughout the rest of the sibling data
  -- , morphism :: Equation -- TODO change this for different mappings - for now, we're linear
  }

derive instance genericTimeScale :: Generic TimeScale _

derive newtype instance eqTimeScale :: Eq TimeScale

derive newtype instance showTimeScale :: Show TimeScale

instance arbitraryTimeScale :: Arbitrary TimeScale where
  arbitrary = do
    name <- genString
    units <- genString
    description <- genString
    limit <- arbitrary
    pure (TimeScale { name, units, description, limit })

instance encodeJsonTimeScale :: EncodeJson TimeScale where
  encodeJson (TimeScale { name, units, description, limit }) =
    "name" := name
      ~> "units"
      := units
      ~> "description"
      := description
      ~> "limit"
      := limit
      ~> jsonEmptyObject

instance decodeJsonTimeScale :: DecodeJson TimeScale where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    units <- o .: "units"
    description <- o .: "description"
    limit <- o .: "limit"
    pure (TimeScale { name, units, description, limit })

instance defaultTimeScale :: Default TimeScale where
  def =
    TimeScale
      { name: "TimeScale Name"
      , units: "Years"
      , description: ""
      , limit: DecidedMaybeLimitNumber NothingLimit
      }
