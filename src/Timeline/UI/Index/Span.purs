module Timeline.UI.Index.Span where

import Timeline.UI.Index.Value (DecidedValue(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either)
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , Json
  , (~>)
  , jsonEmptyObject
  , (:=)
  , (.:)
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)
import Type.Proxy (Proxy(..))

type Span a
  = { start :: a, stop :: a }

genSpan :: forall a. Arbitrary a => Proxy a -> Gen (Span a)
genSpan Proxy = do
  start <- arbitrary
  stop <- arbitrary
  pure { start, stop }

encodeJsonSpan :: forall a. EncodeJson a => Proxy a -> Span a -> Json
encodeJsonSpan Proxy { start, stop } = "start" := start ~> "stop" := stop ~> jsonEmptyObject

decodeJsonSpan :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Span a)
decodeJsonSpan Proxy json = do
  o <- decodeJson json
  start <- o .: "start"
  stop <- o .: "stop"
  pure { start, stop }

data DecidedSpan
  = DecidedSpanNumber (Span Number)

makeDecidedSpan :: { start :: DecidedValue, stop :: DecidedValue } -> Maybe DecidedSpan
makeDecidedSpan { start, stop } = case Tuple start stop of
  Tuple (DecidedValueNumber start') (DecidedValueNumber stop') -> Just (DecidedSpanNumber { start: start', stop: stop' })
  _ -> Nothing

derive instance genericDecidedSpan :: Generic DecidedSpan _

instance eqDecidedSpan :: Eq DecidedSpan where
  eq = genericEq

instance showDecidedSpan :: Show DecidedSpan where
  show = genericShow

instance encodeJsonDecidedSpan :: EncodeJson DecidedSpan where
  encodeJson x = case x of
    DecidedSpanNumber y -> "numberSpan" := encodeJsonSpan (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedSpan :: DecodeJson DecidedSpan where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberSpan"
        DecidedSpanNumber <$> decodeJsonSpan (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedSpan :: Arbitrary DecidedSpan where
  arbitrary = oneOf $ NonEmpty (DecidedSpanNumber <$> genSpan (Proxy :: Proxy Number)) []
