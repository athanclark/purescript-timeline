module Timeline.UI.Index.Unit where

import Prelude
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , fail
  )
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)

data DecidedUnit
  = DecidedUnitNumber
  | DecidedUnitFoo

derive instance genericDecidedUnit :: Generic DecidedUnit _

instance eqDecidedUnit :: Eq DecidedUnit where
  eq = genericEq

instance showDecidedUnit :: Show DecidedUnit where
  show = genericShow

instance encodeJsonDecidedUnit :: EncodeJson DecidedUnit where
  encodeJson x = case x of
    DecidedUnitNumber -> encodeJson "number"
    DecidedUnitFoo -> encodeJson "foo"

instance decodeJsonDecidedUnit :: DecodeJson DecidedUnit where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "number" -> pure DecidedUnitNumber
      "foo" -> pure DecidedUnitFoo
      _ -> fail $ "Unrecognized DecidedUnit: " <> s

instance arbitraryDecidedUnit :: Arbitrary DecidedUnit where
  arbitrary = elements $ NonEmpty DecidedUnitNumber [ DecidedUnitFoo ]
