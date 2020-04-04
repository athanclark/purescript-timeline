module Timeline.Data where

import Prelude
import Data.Eq (class EqRecord)
import Data.Ord (class OrdRecord)
import Data.Maybe (Maybe)
import Data.IxSet (IxSet, Index)
import Data.IxSet (insert, delete, lookup) as Ix
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Effect (Effect)
import Prim.RowList (class RowToList)



-- ------------------ TimeSpace

-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
type TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index =
  { timeScale :: TimeScale timeScaleAux index
  , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) -- ^ Mutable reference
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  | timeSpaceAux }

-- | Alias to not confuse this with a TimeIndex
type TimelineNum = Index


insertTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> Effect TimelineNum
insertTimeline t c = Ix.insert c t.timelines


deleteTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> TimelineNum
               -> Effect Unit
deleteTimeline t i = Ix.delete i t.timelines


lookupTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> TimelineNum
               -> Effect (Maybe (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index))
lookupTimeline t i = Ix.lookup i t.timelines



-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux
  = TimeSpaceNumber (TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux Number)
derive instance genericTimeSpaceDecided :: Generic (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) _
instance eqTimeSpaceDecided :: ( RowToList ( timeScale :: TimeScale timeScaleAux Number
                                           , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux Number) | timeSpaceAux) t1
                               , EqRecord t1 ( timeScale :: TimeScale timeScaleAux Number
                                             , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux Number) | timeSpaceAux)
                               ) => Eq (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) where
  eq = genericEq
-- TODO morphisms between decided types


-- ------------------ TimeScale

-- | Parameters for defining how time is represented spatially and numerically
type TimeScale timeScaleAux index =
  { beginIndex :: index
  , endIndex   :: index
  -- TODO human <-> presented interpolation
  | timeScaleAux }

changeTimeScaleBegin :: forall timeScaleAux index
                      . TimeScale timeScaleAux index -> index
                     -> TimeScale timeScaleAux index
changeTimeScaleBegin t i = t {beginIndex = i}

changeTimeScaleEnd :: forall timeScaleAux index
                    . TimeScale timeScaleAux index -> index
                   -> TimeScale timeScaleAux index
changeTimeScaleEnd t i = t {endIndex = i}



-- ------------------ Timeline

-- | Types of children in a Timeline
data TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
  = EventChild (Event eventAux index)
  | TimeSpanChild (TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)
derive instance genericTimelineChild :: Generic (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _
instance eqTimelineChild :: ( RowToList (timeIndex :: index | eventAux) t1
                            , EqRecord t1 (timeIndex :: index | eventAux)
                            , RowToList ( startIndex :: index, stopIndex :: index
                                        , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux) t2
                            , EqRecord t2 ( startIndex :: index, stopIndex :: index
                                          , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux)
                            ) => Eq (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) where
  eq = genericEq
-- | Only compares on `startIndex` of TimeSpan - not a true ordering!
instance ordTimelineChild :: ( RowToList (timeIndex :: index | eventAux) t1
                             , OrdRecord t1 (timeIndex :: index | eventAux)
                             , RowToList ( startIndex :: index, stopIndex :: index
                                         , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux) t2
                             , OrdRecord t2 ( startIndex :: index, stopIndex :: index
                                            , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux)
                             ) => Ord (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) where
  compare (EventChild x) (EventChild y) = compare x y
  compare (TimeSpanChild x) (TimeSpanChild y) = compare x y
  compare (EventChild x) (TimeSpanChild y) = LT
  compare (TimeSpanChild y) (EventChild x) = GT

-- | Alias to not confuse this with a TimeIndex
type TimelineChildNum = Index

-- | A set of Timeline children - events and timespans
type Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index =
  { children :: IxSet (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) -- ^ Mutable reference
  | timelineAux }


insertTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> Effect TimelineChildNum
insertTimelineChild t c = Ix.insert c t.children


deleteTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChildNum
                    -> Effect Unit
deleteTimelineChild t i = Ix.delete i t.children


lookupTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChildNum
                    -> Effect (Maybe (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index))
lookupTimelineChild t i = Ix.lookup i t.children


-- ------------------ Event

-- | A punctuated event in time
type Event eventAux index =
  { timeIndex :: index
  | eventAux }

-- | **Note**: Does not check for surrounding bounds
moveEvent :: forall eventAux index. Event eventAux index -> index -> Event eventAux index
moveEvent x i = x {timeIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftEvent :: forall eventAux index
                . Ring index
               => Event eventAux index -> index
               -> Event eventAux index
shiftLeftEvent x i = x {timeIndex = x.timeIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightEvent :: forall eventAux index
                 . Semiring index
                => Event eventAux index -> index
                -> Event eventAux index
shiftRightEvent x i = x {timeIndex = x.timeIndex + i}



-- ------------------ TimeSpan

-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
type TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index =
  { startIndex :: index
  , stopIndex :: index
  , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux)
  | timeSpanAux }


-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStart :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStart x i = x {startIndex = i}

-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStop :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStop x i = x {stopIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftTimeSpan :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . Ring index
                  => TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
shiftLeftTimeSpan x i = x {startIndex = x.startIndex - i, stopIndex = x.stopIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightTimeSpan :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    . Semiring index
                   => TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                   -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
shiftRightTimeSpan x i = x {startIndex = x.startIndex + i, stopIndex = x.stopIndex + i}

