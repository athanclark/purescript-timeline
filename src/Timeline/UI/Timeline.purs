module Timeline.UI.Timeline where

import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..))
import Timeline.ID.Timeline (TimelineID(..))
import Timeline.ID.Event (EventID(..))
import Timeline.ID.TimeSpan (TimeSpanID(..))
import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson)
import Data.Default (class Default)
import Data.UUID (genUUID) as UUID
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Random (randomInt)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)
import Partial.Unsafe (unsafePartial)

newtype Timeline
  = Timeline
  { name :: String
  , description :: String
  -- TODO color
  , children :: Array (EventOrTimeSpanPoly EventID TimeSpanID)
  , id :: TimelineID
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
      := children
      ~> "id"
      := id
      ~> jsonEmptyObject

instance decodeJsonTimeline :: DecodeJson Timeline where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    children <- o .: "children"
    id <- o .: "id"
    pure (Timeline { name, description, children, id })

instance arbitraryTimeline :: Arbitrary Timeline where
  arbitrary = do
    name <- genString
    description <- genString
    children <- arbitrary
    id <- arbitrary
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
                  1 -> Left <<< EventID <$> UUID.genUUID
                  2 -> Right <<< TimeSpanID <$> UUID.genUUID
    in
      Timeline
        { name: "Timeline"
        , description: ""
        , children
        , id: TimelineID (unsafePerformEffect UUID.genUUID)
        }

-- newTimelinesSignal ::
--   { timeSpacesMapping :: IxSignalMap TimeSpaceID (read :: S.READ, write :: S.WRITE) TimeSpace
--   , timelinesMapping :: IxSignalMap TimelineID (read :: S.READ, write :: S.WRITE) Timeline
--   , timeSpaceSelectedSignal :: IxSignal (read :: S.READ) (Array TimeSpaceID)
--   , rootRef :: Ref (Maybe TimeSpaceID)
--   } ->
--   Effect (IxSignalArray (read :: S.READ, write :: S.WRITE) Timeline)
-- newTimelinesSignal {timeSpacesMapping, timelinesMapping, timeSpaceSelectedSignal} = do
--   currentTimeSpaceID <- getCurrentTimeSpaceID
--   IxSignalArray.new
--   where
--     getCurrentTimeSpaceID :: Effect TimeSpaceID
--     getCurrentTimeSpaceID = do
--       timeSpaceIDs <- IxSignal.get timeSpaceSelectedSignal
--       case Array.last timeSpaceIDs of
--         Just last -> pure last
--         Nothing -> do
--           mRoot <- Ref.read rootRef
--           case mRoot of
--             Nothing -> throw "No root TimeSpaceID"
--             Just root -> pure root
