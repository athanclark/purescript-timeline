module Timeline.UI.TimeSpace where

import Timeline.UI.TimeScale (TimeScale)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Bifunctor (bimap)
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  , fail
  )
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, arrayOf)
import Test.QuickCheck.UTF8String (genString)

newtype TimeSpace
  = TimeSpace
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , siblings :: Array (EventOrTimeSpanPoly UUID UUID) -- TODO manual field sorting
  , timelines :: Array UUID
  , id :: UUID -- TODO trim the fat later
  }

derive instance genericTimeSpace :: Generic TimeSpace _

derive newtype instance eqTimeSpace :: Eq TimeSpace

derive newtype instance showTimeSpace :: Show TimeSpace

instance encodeJsonTimeSpace :: EncodeJson TimeSpace where
  encodeJson (TimeSpace { title, description, timeScale, siblings, timelines, id }) =
    "title" := title
      ~> "description"
      := description
      ~> "timeScale"
      := timeScale
      ~> "siblings"
      := map (bimap UUID.toString UUID.toString) siblings
      ~> "timelines"
      := map UUID.toString timelines
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: DecodeJson TimeSpace where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    let
      getUUID s = case UUID.parseUUID s of
        Nothing -> fail $ "Couldn't parse UUID: " <> s
        Just x -> pure x

      getIds (EventOrTimeSpanPoly eOrTs) = case eOrTs of
        Left s -> EventOrTimeSpanPoly <<< Left <$> getUUID s
        Right s -> EventOrTimeSpanPoly <<< Right <$> getUUID s
    timeScale <- o .: "timeScale"
    siblings <- o .: "siblings" >>= traverse getIds
    timelines <- o .: "timelines" >>= traverse getUUID
    id <- o .: "id" >>= getUUID
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })

instance arbitraryEvent :: Arbitrary TimeSpace where
  arbitrary = do
    title <- genString
    description <- genString
    timeScale <- arbitrary
    let
      genId = pure (unsafePerformEffect UUID.genUUID)
    siblings <- arrayOf $ EventOrTimeSpanPoly <$> oneOf (NonEmpty (Left <$> genId) [ Right <$> genId ])
    timelines <- arrayOf genId
    id <- genId
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })
