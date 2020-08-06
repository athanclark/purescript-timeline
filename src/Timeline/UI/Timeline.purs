module Timeline.UI.Timeline where

import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..))
import Timeline.ID.TimeSpace (TimeSpaceID(..))
import Timeline.ID.Timeline (TimelineID(..))
import Timeline.ID.Event (EventID(..))
import Timeline.ID.TimeSpan (TimeSpanID(..))
import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson)
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (unsafeFromArray) as UniqueArray
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
  , children :: UniqueArray (EventOrTimeSpanPoly EventID TimeSpanID)
  , id :: TimelineID
  , timeSpace :: TimeSpaceID
  }

derive instance genericTimeline :: Generic Timeline _

derive newtype instance eqTimeline :: Eq Timeline

derive newtype instance showTimeline :: Show Timeline

instance encodeJsonTimeline :: EncodeJson Timeline where
  encodeJson (Timeline { name, description, children, id, timeSpace }) =
    "name" := name
      ~> "description"
      := description
      ~> "children"
      := children
      ~> "id"
      := id
      ~> "timeSpace"
      := timeSpace
      ~> jsonEmptyObject

instance decodeJsonTimeline :: DecodeJson Timeline where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    children <- o .: "children"
    id <- o .: "id"
    timeSpace <- o .: "timeSpace"
    pure (Timeline { name, description, children, id, timeSpace })

instance arbitraryTimeline :: Arbitrary Timeline where
  arbitrary = do
    name <- genString
    description <- genString
    children <- arbitrary
    id <- arbitrary
    timeSpace <- arbitrary
    pure (Timeline { name, description, children, id, timeSpace })

-- | FIXME shouldn't be necessary
instance defaultTimleine :: Default Timeline where
  def =
    let
      children =
        unsafePerformEffect do
          l <- randomInt 1 20
          xs <-
            replicateA l do
              lOrR <- randomInt 1 2
              unsafePartial $ map EventOrTimeSpanPoly
                $ case lOrR of
                    1 -> Left <<< EventID <$> UUID.genUUID
                    2 -> Right <<< TimeSpanID <$> UUID.genUUID
          pure (UniqueArray.unsafeFromArray xs)
    in
      Timeline
        { name: "Timeline"
        , description: ""
        , children
        , id: TimelineID (unsafePerformEffect UUID.genUUID)
        , timeSpace: TimeSpaceID (unsafePerformEffect UUID.genUUID)
        }
