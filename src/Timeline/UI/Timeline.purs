module Timeline.UI.Timeline where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson, fail)
import Data.Default (class Default)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
import Data.Traversable (traverse)
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (randomInt)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf)
import Test.QuickCheck.UTF8String (genString)

newtype Timeline
  = Timeline
  { name :: String
  , description :: String
  -- TODO color
  , eventChildren :: Array UUID
  , timeSpanChildren :: Array UUID
  , id :: UUID
  }

derive instance genericTimeline :: Generic Timeline _

derive newtype instance eqTimeline :: Eq Timeline

derive newtype instance showTimeline :: Show Timeline

instance encodeJsonTimeline :: EncodeJson Timeline where
  encodeJson (Timeline { name, description, eventChildren, timeSpanChildren, id }) =
    "name" := name
      ~> "description"
      := description
      ~> "eventChildren"
      := map UUID.toString eventChildren
      ~> "timeSpanChildren"
      := map UUID.toString timeSpanChildren
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
    eventChildren <- o .: "eventChildren" >>= traverse getUUID
    timeSpanChildren <- o .: "timeSpanChildren" >>= traverse getUUID
    id <- o .: "id" >>= getUUID
    pure (Timeline { name, description, eventChildren, timeSpanChildren, id })

instance arbitraryTimeline :: Arbitrary Timeline where
  arbitrary = do
    name <- genString
    description <- genString
    let
      genUUID = do
        pure (unsafePerformEffect UUID.genUUID)
    eventChildren <- arrayOf genUUID
    timeSpanChildren <- arrayOf genUUID
    id <- genUUID
    pure (Timeline { name, description, eventChildren, timeSpanChildren, id })

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
      eventChildren =
        unsafePerformEffect do
          l <- randomInt 1 20
          replicateA l UUID.genUUID

      timeSpanChildren =
        unsafePerformEffect do
          l <- randomInt 1 20
          replicateA l UUID.genUUID
    in
      Timeline
        { name: "Timeline"
        , description: ""
        , eventChildren
        , timeSpanChildren
        , id: unsafePerformEffect UUID.genUUID
        }
