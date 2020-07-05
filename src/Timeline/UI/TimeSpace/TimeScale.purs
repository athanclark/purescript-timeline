module Timeline.UI.TimeSpace.TimeScale where

import Timeline.Time.MaybeLimit
  ( DecidedMaybeLimit(DecidedMaybeLimitNumber)
  , MaybeLimit(NothingLimit)
  )
import Timeline.UI.Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , (:=)
  , (.:)
  , (~>)
  , jsonEmptyObject
  , stringify
  , jsonParser
  )
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Effect (Effect)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, get, set, subscribeDiffLight)
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
