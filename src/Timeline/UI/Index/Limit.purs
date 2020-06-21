module Timeline.UI.Index.Limit where

import Timeline.UI.Index.Value (DecidedValue(..))
import Timeline.UI.Index.Min (Min)
import Timeline.UI.Index.Max (Max)
import Timeline.UI.Index.Bounds (Bounds)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  )
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data Limit a
  = LimitBounds (Bounds a)
  | LimitMin (Min a)
  | LimitMax (Max a)

derive instance genericLimit :: Generic a a' => Generic (Limit a) _

instance eqLimit :: Eq a => Eq (Limit a) where
  eq x y = case Tuple x y of
    Tuple (LimitBounds x') (LimitBounds y') -> x' == y'
    Tuple (LimitMin x') (LimitMin y') -> x' == y'
    Tuple (LimitMax x') (LimitMax y') -> x' == y'
    _ -> false

instance showLimit :: Show a => Show (Limit a) where
  show x = case x of
    LimitBounds y -> "(LimitBounds " <> show y <> ")"
    LimitMin y -> "(LimitMin " <> show y <> ")"
    LimitMax y -> "(LimitMax " <> show y <> ")"

instance encodeJsonLimit :: EncodeJson a => EncodeJson (Limit a) where
  encodeJson x = case x of
    LimitBounds y -> "limitBounds" := y ~> jsonEmptyObject
    LimitMin y -> "limitMin" := y ~> jsonEmptyObject
    LimitMax y -> "limitMax" := y ~> jsonEmptyObject

instance decodeJsonLimit :: DecodeJson a => DecodeJson (Limit a) where
  decodeJson json = do
    o <- decodeJson json
    let
      limitBounds = LimitBounds <$> o .: "limitBounds"

      limitMin = LimitMin <$> o .: "limitMin"

      limitMax = LimitMax <$> o .: "limitMax"
    limitBounds <|> limitMin <|> limitMax

instance arbitraryLimit :: Arbitrary a => Arbitrary (Limit a) where
  arbitrary = oneOf $ NonEmpty (LimitBounds <$> arbitrary) [ LimitMin <$> arbitrary, LimitMax <$> arbitrary ]

data DecidedLimit
  = DecidedLimitNumber (Limit Number)

makeDecidedLimit :: { begin :: Maybe DecidedValue, end :: Maybe DecidedValue } -> Maybe DecidedLimit
makeDecidedLimit { begin, end } = case Tuple begin end of
  Tuple (Just (DecidedValueNumber begin')) (Just (DecidedValueNumber end')) -> Just (DecidedLimitNumber (LimitBounds { begin: begin', end: end' }))
  Tuple (Just (DecidedValueNumber begin')) Nothing -> Just (DecidedLimitNumber (LimitMin { begin: begin' }))
  Tuple Nothing (Just (DecidedValueNumber end')) -> Just (DecidedLimitNumber (LimitMax { end: end' }))
  _ -> Nothing -- TODO other units

unmakeDecidedLimit :: DecidedLimit -> { begin :: Maybe DecidedValue, end :: Maybe DecidedValue }
unmakeDecidedLimit l = case l of
  DecidedLimitNumber l' -> case l' of
    LimitBounds { begin, end } -> { begin: Just (DecidedValueNumber begin), end: Just (DecidedValueNumber end) }
    LimitMin { begin } -> { begin: Just (DecidedValueNumber begin), end: Nothing }
    LimitMax { end } -> { begin: Nothing, end: Just (DecidedValueNumber end) }

derive instance genericDecidedLimit :: Generic DecidedLimit _

instance eqDecidedLimit :: Eq DecidedLimit where
  eq = genericEq

instance showDecidedLimit :: Show DecidedLimit where
  show = genericShow

instance encodeJsonDecidedLimit :: EncodeJson DecidedLimit where
  encodeJson x = case x of
    DecidedLimitNumber y -> "numberLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedLimit :: DecodeJson DecidedLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedLimitNumber <$> o .: "numberLimit"
    decodeNumber

instance arbitraryDecidedLimit :: Arbitrary DecidedLimit where
  arbitrary = oneOf $ NonEmpty (DecidedLimitNumber <$> arbitrary) []
