module Timeline.UI.Index.Min where

import Timeline.UI.Index.Value (DecidedValue(..))
import Prelude
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

type Min a
  = { begin :: a }

genMin :: forall a. Arbitrary a => Proxy a -> Gen (Min a)
genMin Proxy = do
  begin <- arbitrary
  pure { begin }

encodeJsonMin :: forall a. EncodeJson a => Proxy a -> Min a -> Json
encodeJsonMin Proxy { begin } = "begin" := begin ~> jsonEmptyObject

decodeJsonMin :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Min a)
decodeJsonMin Proxy json = do
  o <- decodeJson json
  begin <- o .: "begin"
  pure { begin }

data DecidedMin
  = DecidedMinNumber (Min Number)

makeDecidedMin :: DecidedValue -> DecidedMin
makeDecidedMin v = case v of
  DecidedValueNumber n -> DecidedMinNumber { begin: n }

derive instance genericDecidedMin :: Generic DecidedMin _

instance eqDecidedMin :: Eq DecidedMin where
  eq = genericEq

instance showDecidedMin :: Show DecidedMin where
  show = genericShow

instance encodeJsonDecidedMin :: EncodeJson DecidedMin where
  encodeJson x = case x of
    DecidedMinNumber y -> "numberMin" := encodeJsonMin (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMin :: DecodeJson DecidedMin where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMin"
        DecidedMinNumber <$> decodeJsonMin (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedMin :: Arbitrary DecidedMin where
  arbitrary = oneOf $ NonEmpty (DecidedMinNumber <$> genMin (Proxy :: Proxy Number)) []
