module Timeline.Convert.Errors where

import Timeline.UI.TimeSpace (TimeSpace) as UI
import Timeline.UI.Timeline (Timeline) as UI
import Timeline.UI.Event (Event) as UI
import Timeline.UI.TimeSpan (TimeSpan) as UI
import Timeline.Data (TimeScale) as Data
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.Time.Unit (DecidedUnit)
import Timeline.Time.Value (DecidedValue)
import Timeline.Time.Span (Span)
import Prelude
import Data.UUID (UUID)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data PopulateError
  = ConvertTimeScaleFailed
    { decidedUnit :: DecidedUnit
    , timeScale :: Data.TimeScale DecidedValue
    }
  | TimeSpaceExists { timeSpace :: UI.TimeSpace, sets :: String }
  | TimelineExists UI.Timeline
  | SiblingEventExists UI.Event
  | SiblingTimeSpanExists UI.TimeSpan
  | SiblingTimeSpanMakeSpanFailed (Span DecidedValue)
  | ChildEventExists UI.Event
  | ChildTimeSpanExists UI.TimeSpan
  | ChildTimeSpanMakeSpanFailed (Span DecidedValue)

derive instance genericPopulateError :: Generic PopulateError _

instance showPopulateError :: Show PopulateError where
  show = genericShow

data SynthesizeError
  = TimeSpaceDoesntExist TimeSpaceID
  | TimelineDoesntExist UUID
  | SiblingEventDoesntExist UUID
  | SiblingTimeSpanDoesntExist UUID
  | ChildEventDoesntExist UUID
  | ChildTimeSpanDoesntExists UUID
  | NoRootExists
  | ConvertDecidedValueError
    { decidedUnit :: DecidedUnit
    , decidedValue :: DecidedValue
    }

derive instance genericSynthesizeError :: Generic SynthesizeError _

instance showSynthesizeError :: Show SynthesizeError where
  show = genericShow
