module Timeline.Data where

import Prelude
import Data.Maybe (Maybe)
import Data.IxSet (IxSet, Index)
import Data.IxSet (insert, delete, lookup) as Ix
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Effect (Effect)



-- ------------------ TimeSpace

-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
newtype TimeSpace index = TimeSpace
  { timeScale :: TimeScale index
  , timelines :: IxSet (Timeline index) -- ^ Mutable reference
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  -- non-essential
  , title       :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO markers, metrics & graduation
  }
derive instance genericTimeSpace :: Generic (TimeSpace index) _
derive newtype instance eqTimeSpace :: Ord index => Eq (TimeSpace index)
derive newtype instance ordTimeSpace :: Ord index => Ord (TimeSpace index)

-- | Alias to not confuse this with a TimeIndex
type TimelineNum = Index


insertTimeline :: forall index
                . TimeSpace index
               -> Timeline index
               -> Effect TimelineNum
insertTimeline (TimeSpace t) c = Ix.insert c t.timelines


deleteTimeline :: forall index
                . TimeSpace index
               -> TimelineNum
               -> Effect Unit
deleteTimeline (TimeSpace t) i = Ix.delete i t.timelines


lookupTimeline :: forall index
                . TimeSpace index
               -> TimelineNum
               -> Effect (Maybe (Timeline index))
lookupTimeline (TimeSpace t) i = Ix.lookup i t.timelines



-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided
  = TimeSpaceNumber (TimeSpace Number)
derive instance genericTimeSpaceDecided :: Generic TimeSpaceDecided _
instance eqTimeSpaceDecided :: Eq TimeSpaceDecided where
  eq = genericEq
instance ordTimeSpaceDecided :: Ord TimeSpaceDecided where
  compare = genericCompare


-- TODO morphisms between decided types


-- ------------------ TimeScale

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index = TimeScale
  { beginIndex :: index
  , endIndex   :: index
  -- TODO human <-> presented interpolation
  -- non-essential
  , timeScaleName :: String
  , description   :: String
  , units         :: String
  , document      :: Maybe String -- TODO markdown
  }
derive instance genericTimeScale :: Generic (TimeScale index) _
derive newtype instance eqTimeScale :: Eq index => Eq (TimeScale index)
derive newtype instance ordTimeScale :: Ord index => Ord (TimeScale index)

changeTimeScaleBegin :: forall index
                      . TimeScale index -> index
                     -> TimeScale index
changeTimeScaleBegin (TimeScale t) i = TimeScale t {beginIndex = i}

changeTimeScaleEnd :: forall index
                    . TimeScale index -> index
                   -> TimeScale index
changeTimeScaleEnd (TimeScale t) i = TimeScale t {endIndex = i}



-- ------------------ Timeline

-- | Types of children in a Timeline
data TimelineChild index
  = EventChild (Event index)
  | TimeSpanChild (TimeSpan index)
derive instance genericTimelineChild :: Generic (TimelineChild index) _
instance eqTimelineChild :: Eq index => Eq (TimelineChild index) where
  eq = genericEq
instance ordTimelineChild :: Ord index => Ord (TimelineChild index) where
  compare = genericCompare

-- | Alias to not confuse this with a TimeIndex
type TimelineChildNum = Index

-- | A set of Timeline children - events and timespans
newtype Timeline index = Timeline
  { children :: IxSet (TimelineChild index) -- ^ Mutable reference
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  }
derive instance genericTimeline :: Generic (Timeline index) _
derive newtype instance eqTimeline :: Ord index => Eq (Timeline index)
derive newtype instance ordTimeline :: Ord index => Ord (Timeline index)


insertTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChild index
                    -> Effect TimelineChildNum
insertTimelineChild (Timeline t) c = Ix.insert c t.children


deleteTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChildNum
                    -> Effect Unit
deleteTimelineChild (Timeline t) i = Ix.delete i t.children


lookupTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChildNum
                    -> Effect (Maybe (TimelineChild index))
lookupTimelineChild (Timeline t) i = Ix.lookup i t.children


-- ------------------ Event

-- | A punctuated event in time
newtype Event index = Event
  { timeIndex :: index
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  }
derive instance genericEvent :: Generic (Event index) _
derive newtype instance eqEvent :: Eq index => Eq (Event index)
derive newtype instance ordEvent :: Ord index => Ord (Event index)

-- | **Note**: Does not check for surrounding bounds
moveEvent :: forall index. Event index -> index -> Event index
moveEvent (Event x) i = Event x {timeIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftEvent :: forall index
                . Ring index
               => Event index -> index
               -> Event index
shiftLeftEvent (Event x) i = Event x {timeIndex = x.timeIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightEvent :: forall index
                 . Semiring index
                => Event index -> index
                -> Event index
shiftRightEvent (Event x) i = Event x {timeIndex = x.timeIndex + i}



-- ------------------ TimeSpan

-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan index = TimeSpan
  { startIndex :: index
  , stopIndex :: index
  , timeSpace :: Maybe TimeSpaceDecided
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  }
derive instance genericTimeSpan :: Generic (TimeSpan index) _
derive newtype instance eqTimeSpan :: Eq index => Eq (TimeSpan index)
derive newtype instance ordTimeSpan :: Ord index => Ord (TimeSpan index)


-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStart :: forall index
                   . TimeSpan index -> index
                  -> TimeSpan index
moveTimeSpanStart (TimeSpan x) i = TimeSpan x {startIndex = i}

-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStop :: forall index
                   . TimeSpan index -> index
                  -> TimeSpan index
moveTimeSpanStop (TimeSpan x) i = TimeSpan x {stopIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftTimeSpan :: forall index
                   . Ring index
                  => TimeSpan index -> index
                  -> TimeSpan index
shiftLeftTimeSpan (TimeSpan x) i = TimeSpan x {startIndex = x.startIndex - i, stopIndex = x.stopIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightTimeSpan :: forall index
                    . Semiring index
                   => TimeSpan index -> index
                   -> TimeSpan index
shiftRightTimeSpan (TimeSpan x) i = TimeSpan x {startIndex = x.startIndex + i, stopIndex = x.stopIndex + i}

