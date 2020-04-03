module Timeline.Data where

import Prelude
import Data.Maybe (Maybe)
import Data.IxSet (IxSet)
import Data.Generic.Rep (class Generic)



-- ------------------ TimeSpace


newtype TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = TimeSpace
  { timeScale :: TimeScale timeScaleAux index
  , timelines :: Array (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)
  | timeSpaceAux }
derive instance genericTimeSpace :: Generic (TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _



-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux
  = TimeSpaceNumber (TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux Number)
derive instance genericTimeSpaceDecided :: Generic (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) _



-- ------------------ TimeScale

newtype TimeScale timeScaleAux index = TimeScale
  { beginIndex :: index
  , endIndex   :: index
  -- TODO human <-> presented interpolation
  | timeScaleAux }
derive instance genericTimeScale :: Generic (TimeScale timeScaleAux index) _



-- ------------------ Timeline

-- | Types of children in a Timeline
data TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
  = EventChild (Event eventAux index)
  | TimeSpanChild (TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)
derive instance genericTimelineChild :: Generic (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _


newtype Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = Timeline
  { children :: IxSet (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)
  | timelineAux }
derive instance genericTimeline :: Generic (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _


-- ------------------ Event

newtype Event eventAux index = Event
  { timeIndex :: index
  | eventAux }
derive instance genericEvent :: Generic (Event eventAux index) _

moveEvent :: forall eventAux index. Event eventAux index -> index -> Event eventAux index
moveEvent (Event x) i = Event (x {timeIndex = i})

-- FIXME shift by...?


-- ------------------ TimeSpan

newtype TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = TimeSpan
  { startIndex :: index
  , stopIndex :: index
  , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux)
  | timeSpanAux }
derive instance genericTimeSpan :: Generic (TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _


moveTimeSpanStart :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStart (TimeSpan x) i = TimeSpan (x {startIndex = i})

moveTimeSpanStop :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStop (TimeSpan x) i = TimeSpan (x {stopIndex = i})

-- TODO shift left / right, scale?
