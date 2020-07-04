module Timeline.UI.EventOrTimeSpan where

import Timeline.UI.Event (Event)
import Timeline.UI.TimeSpan (TimeSpan)
import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Bifunctor (class Bifunctor)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  )
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary)

newtype EventOrTimeSpan
  = EventOrTimeSpan (Either Event TimeSpan)

derive instance genericEventOrTimeSpan :: Generic EventOrTimeSpan _

derive newtype instance eqEventOrTimeSpan :: Eq EventOrTimeSpan

derive newtype instance showEventOrTimeSpan :: Show EventOrTimeSpan

instance encodeJsonEventOrTimeSpan :: EncodeJson EventOrTimeSpan where
  encodeJson (EventOrTimeSpan eOrT) = case eOrT of
    Left e -> "event" := e ~> jsonEmptyObject
    Right t -> "timeSpan" := t ~> jsonEmptyObject

instance decodeJsonEventOrTimeSpan :: DecodeJson EventOrTimeSpan where
  decodeJson json = do
    o <- decodeJson json
    let
      event = Left <$> (o .: "event")

      timeSpan = Right <$> (o .: "timeSpan")
    EventOrTimeSpan <$> (event <|> timeSpan)

derive newtype instance arbitraryEventOrTimeSpan :: Arbitrary EventOrTimeSpan

newtype EventOrTimeSpanPoly a b
  = EventOrTimeSpanPoly (Either a b)

derive instance genericEventOrTimeSpanPoly :: (Generic a a', Generic b b') => Generic (EventOrTimeSpanPoly a b) _

derive newtype instance eqEventOrTimeSpanPoly :: (Eq a, Eq b) => Eq (EventOrTimeSpanPoly a b)

derive newtype instance showEventOrTimeSpanPoly :: (Show a, Show b) => Show (EventOrTimeSpanPoly a b)

derive newtype instance functorEventOrTimeSpanPoly :: Functor (EventOrTimeSpanPoly a)

derive newtype instance bifunctorEventOrTimeSpanPoly :: Bifunctor EventOrTimeSpanPoly

instance encodeJsonEventOrTimeSpanPoly :: (EncodeJson a, EncodeJson b) => EncodeJson (EventOrTimeSpanPoly a b) where
  encodeJson (EventOrTimeSpanPoly eOrT) = case eOrT of
    Left e -> "event" := e ~> jsonEmptyObject
    Right t -> "timeSpan" := t ~> jsonEmptyObject

instance decodeJsonEventOrTimeSpanPoly :: (DecodeJson a, DecodeJson b) => DecodeJson (EventOrTimeSpanPoly a b) where
  decodeJson json = do
    o <- decodeJson json
    let
      event = Left <$> (o .: "event")

      timeSpan = Right <$> (o .: "timeSpan")
    EventOrTimeSpanPoly <$> (event <|> timeSpan)

derive newtype instance arbitraryEventOrTimeSpanPoly :: (Arbitrary a, Arbitrary b) => Arbitrary (EventOrTimeSpanPoly a b)
