module Timeline.UI.Index.Bounds where

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

type Bounds a
  = { begin :: a, end :: a }

genBounds :: forall a. Arbitrary a => Proxy a -> Gen (Bounds a)
genBounds Proxy = do
  begin <- arbitrary
  end <- arbitrary
  pure { begin, end }

encodeJsonBounds :: forall a. EncodeJson a => Proxy a -> Bounds a -> Json
encodeJsonBounds Proxy { begin, end } = "begin" := begin ~> "end" := end ~> jsonEmptyObject

decodeJsonBounds :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Bounds a)
decodeJsonBounds Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  end <- o .: "end"
  pure { begin, end }

data DecidedBounds
  = DecidedBoundsNumber (Bounds Number)

makeDecidedBounds :: { begin :: DecidedValue, end :: DecidedValue } -> Maybe DecidedBounds
makeDecidedBounds { begin, end } = case Tuple begin end of
  Tuple (DecidedValueNumber begin') (DecidedValueNumber end') -> Just (DecidedBoundsNumber { begin: begin', end: end' })
  _ -> Nothing

unmakeDecidedBounds :: DecidedBounds -> { begin :: DecidedValue, end :: DecidedValue }
unmakeDecidedBounds b = case b of
  DecidedBoundsNumber { begin, end } -> { begin: DecidedValueNumber begin, end: DecidedValueNumber end }

derive instance genericDecidedBounds :: Generic DecidedBounds _

instance eqDecidedBounds :: Eq DecidedBounds where
  eq = genericEq

instance showDecidedBounds :: Show DecidedBounds where
  show = genericShow

instance encodeJsonDecidedBounds :: EncodeJson DecidedBounds where
  encodeJson x = case x of
    DecidedBoundsNumber y -> "numberBounds" := encodeJsonBounds (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedBounds :: DecodeJson DecidedBounds where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberBounds"
        DecidedBoundsNumber <$> decodeJsonBounds (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedBounds :: Arbitrary DecidedBounds where
  arbitrary = oneOf $ NonEmpty (DecidedBoundsNumber <$> genBounds (Proxy :: Proxy Number)) []
