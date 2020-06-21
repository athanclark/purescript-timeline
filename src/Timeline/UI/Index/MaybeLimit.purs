module Timeline.UI.Index.MaybeLimit where

import Timeline.UI.Index.Unit (DecidedUnit(..))
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
  , encodeJson
  , decodeJson
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  , fail
  )
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data MaybeLimit a
  = JustLimitBounds (Bounds a)
  | JustLimitMin (Min a)
  | JustLimitMax (Max a)
  | NothingLimit

derive instance genericMaybeLimit :: Generic a a' => Generic (MaybeLimit a) _

instance eqMaybeLimit :: Eq a => Eq (MaybeLimit a) where
  eq x y = case Tuple x y of
    Tuple NothingLimit NothingLimit -> true
    Tuple (JustLimitBounds x') (JustLimitBounds y') -> x' == y'
    Tuple (JustLimitMin x') (JustLimitMin y') -> x' == y'
    Tuple (JustLimitMax x') (JustLimitMax y') -> x' == y'
    _ -> false

instance showMaybeLimit :: Show a => Show (MaybeLimit a) where
  show x = case x of
    JustLimitBounds y -> "(JustLimitBounds " <> show y <> ")"
    JustLimitMin y -> "(JustLimitMin " <> show y <> ")"
    JustLimitMax y -> "(JustLimitMax " <> show y <> ")"
    NothingLimit -> "NothingLimit"

instance encodeJsonMaybeLimit :: EncodeJson a => EncodeJson (MaybeLimit a) where
  encodeJson x = case x of
    JustLimitBounds y -> "justLimitBounds" := y ~> jsonEmptyObject
    JustLimitMin y -> "justLimitMin" := y ~> jsonEmptyObject
    JustLimitMax y -> "justLimitMax" := y ~> jsonEmptyObject
    NothingLimit -> encodeJson "nothingLimit"

instance decodeJsonMaybeLimit :: DecodeJson a => DecodeJson (MaybeLimit a) where
  decodeJson json = do
    let
      obj = do
        o <- decodeJson json
        let
          limitBounds = JustLimitBounds <$> o .: "justLimitBounds"

          limitMin = JustLimitMin <$> o .: "justLimitMin"

          limitMax = JustLimitMax <$> o .: "justLimitMax"
        limitBounds <|> limitMin <|> limitMax

      str = do
        s <- decodeJson json
        case s of
          "nothingLimit" -> pure NothingLimit
          _ -> fail $ "Unrecognized MaybeLimit: " <> s
    obj <|> str

instance arbitraryMaybeLimit :: Arbitrary a => Arbitrary (MaybeLimit a) where
  arbitrary = oneOf $ NonEmpty (JustLimitBounds <$> arbitrary) [ JustLimitMin <$> arbitrary, JustLimitMax <$> arbitrary, pure NothingLimit ]

data DecidedMaybeLimit
  = DecidedMaybeLimitNumber (MaybeLimit Number)

-- FIXME should get value from index in explored timespace, if it exists.
-- newTimeScaleUnitSignal :: Effect (IxSig.IxSignal (read :: S.READ, write :: S.WRITE) DecidedMaybeLimit)
-- newTimeScaleUnitSignal = IxSig.make (DecidedMaybeLimitNumber NothingLimit)
makeDecidedMaybeLimit :: { begin :: Maybe DecidedValue, end :: Maybe DecidedValue } -> DecidedMaybeLimit
makeDecidedMaybeLimit { begin, end } = case Tuple begin end of
  Tuple (Just (DecidedValueNumber begin')) (Just (DecidedValueNumber end')) -> DecidedMaybeLimitNumber (JustLimitBounds { begin: begin', end: end' })
  Tuple (Just (DecidedValueNumber begin')) Nothing -> DecidedMaybeLimitNumber (JustLimitMin { begin: begin' })
  Tuple Nothing (Just (DecidedValueNumber end')) -> DecidedMaybeLimitNumber (JustLimitMax { end: end' })
  Tuple Nothing Nothing -> DecidedMaybeLimitNumber NothingLimit -- FIXME how would I know the unit?

unmakeDecidedMaybeLimit :: DecidedMaybeLimit -> { begin :: Maybe DecidedValue, end :: Maybe DecidedValue }
unmakeDecidedMaybeLimit l = case l of
  DecidedMaybeLimitNumber l' -> case l' of
    JustLimitBounds { begin, end } -> { begin: Just (DecidedValueNumber begin), end: Just (DecidedValueNumber end) }
    JustLimitMin { begin } -> { begin: Just (DecidedValueNumber begin), end: Nothing }
    JustLimitMax { end } -> { begin: Nothing, end: Just (DecidedValueNumber end) }
    NothingLimit -> { begin: Nothing, end: Nothing } -- FIXME how would I show the unit?

getMaybeLimitDecidedUnit :: DecidedMaybeLimit -> DecidedUnit
getMaybeLimitDecidedUnit l = case l of
  DecidedMaybeLimitNumber _ -> DecidedUnitNumber

derive instance genericDecidedMaybeLimit :: Generic DecidedMaybeLimit _

instance eqDecidedMaybeLimit :: Eq DecidedMaybeLimit where
  eq = genericEq

instance showDecidedMaybeLimit :: Show DecidedMaybeLimit where
  show = genericShow

instance encodeJsonDecidedMaybeLimit :: EncodeJson DecidedMaybeLimit where
  encodeJson x = case x of
    DecidedMaybeLimitNumber y -> "numberMaybeLimit" := y ~> jsonEmptyObject

instance decodeJsonDecidedMaybeLimit :: DecodeJson DecidedMaybeLimit where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = DecidedMaybeLimitNumber <$> o .: "numberMaybeLimit"
    decodeNumber

instance arbitraryDecidedMaybeLimit :: Arbitrary DecidedMaybeLimit where
  arbitrary = oneOf $ NonEmpty (DecidedMaybeLimitNumber <$> arbitrary) []
