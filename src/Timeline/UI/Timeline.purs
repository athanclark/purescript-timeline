module Timeline.UI.Timeline where

import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson, fail)
import Data.Default (class Default)
import Data.Bifunctor (bimap)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (randomInt)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf, oneOf)
import Test.QuickCheck.UTF8String (genString)
import Partial.Unsafe (unsafePartial)

newtype Timeline
  = Timeline
  { name :: String
  , description :: String
  -- TODO color
  , children :: Array (EventOrTimeSpanPoly UUID UUID)
  , id :: UUID
  }

derive instance genericTimeline :: Generic Timeline _

derive newtype instance eqTimeline :: Eq Timeline

derive newtype instance showTimeline :: Show Timeline

instance encodeJsonTimeline :: EncodeJson Timeline where
  encodeJson (Timeline { name, description, children, id }) =
    "name" := name
      ~> "description"
      := description
      ~> "children"
      := map (bimap UUID.toString UUID.toString) children
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeline :: DecodeJson Timeline where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    let
      getUUID s = case UUID.parseUUID s of
        Nothing -> fail $ "Couldn't parse UUID: " <> s
        Just x -> pure x

      getUUIDs (EventOrTimeSpanPoly eX) =
        EventOrTimeSpanPoly
          <$> case eX of
              Left s -> Left <$> getUUID s
              Right s -> Right <$> getUUID s
    children <- o .: "children" >>= traverse getUUIDs
    id <- o .: "id" >>= getUUID
    pure (Timeline { name, description, children, id })

instance arbitraryTimeline :: Arbitrary Timeline where
  arbitrary = do
    name <- genString
    description <- genString
    let
      genUUID = do
        pure (unsafePerformEffect UUID.genUUID)
    children <-
      arrayOf
        $ EventOrTimeSpanPoly
        <$> oneOf (NonEmpty (Left <$> genUUID) [ Right <$> genUUID ])
    id <- genUUID
    pure (Timeline { name, description, children, id })

-- -- | The key in the IxSignal that listens to changes
-- localstorageSignalKey :: String
-- localstorageSignalKey = "localstorage"
-- FIXME the only thing that should be stored, is the top-level Timeline.Data (TimeSpaceDecided) - all changes to
-- subsidiary signals bubble up to this signal, which gets stored at the top level.
-- localstorageKey :: String
-- localstorageKey = "Timeline"
instance defaultTimleine :: Default Timeline where
  def =
    let
      children =
        unsafePerformEffect do
          l <- randomInt 1 20
          replicateA l do
            lOrR <- randomInt 1 2
            unsafePartial $ map EventOrTimeSpanPoly
              $ case lOrR of
                  1 -> Left <$> UUID.genUUID
                  2 -> Right <$> UUID.genUUID
    in
      Timeline
        { name: "Timeline"
        , description: ""
        , children
        , id: unsafePerformEffect UUID.genUUID
        }
