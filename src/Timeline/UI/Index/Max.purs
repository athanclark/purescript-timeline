module Timeline.UI.Index.Max where

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

type Max a
  = { end :: a }

genMax :: forall a. Arbitrary a => Proxy a -> Gen (Max a)
genMax Proxy = do
  end <- arbitrary
  pure { end }

encodeJsonMax :: forall a. EncodeJson a => Proxy a -> Max a -> Json
encodeJsonMax Proxy { end } = "end" := end ~> jsonEmptyObject

decodeJsonMax :: forall a. DecodeJson a => Proxy a -> Json -> Either String (Max a)
decodeJsonMax Proxy json = do
  o <- decodeJson json
  end <- o .: "end"
  pure { end }

data DecidedMax
  = DecidedMaxNumber (Max Number)

makeDecidedMax :: DecidedValue -> DecidedMax
makeDecidedMax v = case v of
  DecidedValueNumber n -> DecidedMaxNumber { end: n }

derive instance genericDecidedMax :: Generic DecidedMax _

instance eqDecidedMax :: Eq DecidedMax where
  eq = genericEq

instance showDecidedMax :: Show DecidedMax where
  show = genericShow

instance encodeJsonDecidedMax :: EncodeJson DecidedMax where
  encodeJson x = case x of
    DecidedMaxNumber y -> "numberMax" := encodeJsonMax (Proxy :: Proxy Number) y ~> jsonEmptyObject

instance decodeJsonDecidedMax :: DecodeJson DecidedMax where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = do
        j <- o .: "numberMax"
        DecidedMaxNumber <$> decodeJsonMax (Proxy :: Proxy Number) j
    decodeNumber

instance arbitraryDecidedMax :: Arbitrary DecidedMax where
  arbitrary = oneOf $ NonEmpty (DecidedMaxNumber <$> genMax (Proxy :: Proxy Number)) []
