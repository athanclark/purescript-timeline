module Timeline.Data where

import Prelude
import Data.Eq (class EqRecord)
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
newtype TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = TimeSpace
  { timeScale :: TimeScale timeScaleAux index
  , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) -- ^ Mutable reference
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  | timeSpaceAux }
derive instance genericTimeSpace :: Generic (TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _
derive newtype instance eqTimeSpace :: ( RowToList ( timeScale :: TimeScale timeScaleAux index
                                                   , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) | timeSpaceAux) t
                                       , EqRecord t ( timeScale :: TimeScale timeScaleAux index
                                                    , timelines :: IxSet (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) | timeSpaceAux)
                                       ) => Eq (TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)

-- | Alias to not confuse this with a TimeIndex
type TimelineNum = Index


insertTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> Effect TimelineNum
insertTimeline (TimeSpace t) c = Ix.insert c t.timelines


deleteTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> TimelineNum
               -> Effect Unit
deleteTimeline (TimeSpace t) i = Ix.delete i t.timelines


lookupTimeline :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                . TimeSpace timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
               -> TimelineNum
               -> Effect (Maybe (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index))
lookupTimeline (TimeSpace t) i = Ix.lookup i t.timelines



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



-- ------------------ TimeScale

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale timeScaleAux index = TimeScale
  { beginIndex :: index
  , endIndex   :: index
  -- TODO human <-> presented interpolation
  | timeScaleAux }
derive instance genericTimeScale :: Generic (TimeScale timeScaleAux index) _
derive newtype instance eqTimeScale :: ( RowToList (beginIndex :: index, endIndex :: index | timeScaleAux) t
                                       , EqRecord t (beginIndex :: index, endIndex :: index | timeScaleAux)
                                       ) => Eq (TimeScale timeScaleAux index)

changeTimeScaleBegin :: forall timeScaleAux index
                      . TimeScale timeScaleAux index -> index
                     -> TimeScale timeScaleAux index
changeTimeScaleBegin (TimeScale t) i = TimeScale (t {beginIndex = i})

changeTimeScaleEnd :: forall timeScaleAux index
                    . TimeScale timeScaleAux index -> index
                   -> TimeScale timeScaleAux index
changeTimeScaleEnd (TimeScale t) i = TimeScale (t {endIndex = i})



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

-- | Alias to not confuse this with a TimeIndex
type TimelineChildNum = Index

-- | A set of Timeline children - events and timespans
newtype Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = Timeline
  { children :: IxSet (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) -- ^ Mutable reference
  | timelineAux }
derive instance genericTimeline :: Generic (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _
derive newtype instance eqTimeline :: ( RowToList (children :: IxSet (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) | timelineAux) t
                                      , EqRecord t (children :: IxSet (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) | timelineAux)
                                      ) => Eq (Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)


insertTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> Effect TimelineChildNum
insertTimelineChild (Timeline t) c = Ix.insert c t.children


deleteTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChildNum
                    -> Effect Unit
deleteTimelineChild (Timeline t) i = Ix.delete i t.children


lookupTimelineChild :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                     . Timeline timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    -> TimelineChildNum
                    -> Effect (Maybe (TimelineChild timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index))
lookupTimelineChild (Timeline t) i = Ix.lookup i t.children


-- ------------------ Event

-- | A punctuated event in time
newtype Event eventAux index = Event
  { timeIndex :: index
  | eventAux }
derive instance genericEvent :: Generic (Event eventAux index) _
derive newtype instance eqEvent :: ( RowToList (timeIndex :: index | eventAux) t
                                   , EqRecord t (timeIndex :: index | eventAux)
                                   ) => Eq (Event eventAux index)

-- | **Note**: Does not check for surrounding bounds
moveEvent :: forall eventAux index. Event eventAux index -> index -> Event eventAux index
moveEvent (Event x) i = Event (x {timeIndex = i})

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftEvent :: forall eventAux index
                . Ring index
               => Event eventAux index -> index
               -> Event eventAux index
shiftLeftEvent (Event x) i = Event (x {timeIndex = x.timeIndex - i})

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightEvent :: forall eventAux index
                 . Semiring index
                => Event eventAux index -> index
                -> Event eventAux index
shiftRightEvent (Event x) i = Event (x {timeIndex = x.timeIndex + i})



-- ------------------ TimeSpan

-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index = TimeSpan
  { startIndex :: index
  , stopIndex :: index
  , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux)
  | timeSpanAux }
derive instance genericTimeSpan :: Generic (TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index) _
derive newtype instance eqTimeSpan :: ( RowToList ( startIndex :: index, stopIndex :: index
                                                  , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux) t1
                                      , EqRecord t1 ( startIndex :: index, stopIndex :: index
                                                    , timeSpace :: Maybe (TimeSpaceDecided timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux) | timeSpanAux)
                                      ) => Eq (TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index)


-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStart :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStart (TimeSpan x) i = TimeSpan (x {startIndex = i})

-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStop :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
moveTimeSpanStop (TimeSpan x) i = TimeSpan (x {stopIndex = i})

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftTimeSpan :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                   . Ring index
                  => TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                  -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
shiftLeftTimeSpan (TimeSpan x) i = TimeSpan (x {startIndex = x.startIndex - i, stopIndex = x.stopIndex - i})

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightTimeSpan :: forall timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
                    . Semiring index
                   => TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index -> index
                   -> TimeSpan timeSpaceAux timelineAux timeScaleAux eventAux timeSpanAux index
shiftRightTimeSpan (TimeSpan x) i = TimeSpan (x {startIndex = x.startIndex + i, stopIndex = x.stopIndex + i})

