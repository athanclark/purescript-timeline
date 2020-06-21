module Timeline.UI.Timeline where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, (:=), (~>), jsonEmptyObject, (.:), decodeJson)
import Data.Default (class Default)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.UTF8String (genString)

newtype Timeline
  = Timeline
  { name :: String
  , description :: String
  -- TODO color
  }

derive instance genericTimeline :: Generic Timeline _

derive newtype instance eqTimeline :: Eq Timeline

derive newtype instance showTimeline :: Show Timeline

instance encodeJsonTimeline :: EncodeJson Timeline where
  encodeJson (Timeline { name, description }) =
    "name" := name
      ~> "description"
      := description
      ~> jsonEmptyObject

instance decodeJsonTimeline :: DecodeJson Timeline where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    pure (Timeline { name, description })

instance arbitraryTimeline :: Arbitrary Timeline where
  arbitrary = do
    name <- genString
    description <- genString
    pure (Timeline { name, description })

-- -- | The key in the IxSignal that listens to changes
-- localstorageSignalKey :: String
-- localstorageSignalKey = "localstorage"
-- FIXME the only thing that should be stored, is the top-level Timeline.Data (TimeSpaceDecided) - all changes to
-- subsidiary signals bubble up to this signal, which gets stored at the top level.
-- localstorageKey :: String
-- localstorageKey = "Timeline"
instance defaultTimleine :: Default Timeline where
  def =
    Timeline
      { name: "Timeline"
      , description: ""
      }
