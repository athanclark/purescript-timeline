module Timeline.Convert.UISets where

import Timeline.Convert.UISets.TimeSpaces
  ( getTimeSpaceScoped
  , addTimeSpaceScoped
  , addTimeSpaceForceScoped
  , appendTimelineScopedExcept
  , removeTimelineScopedExcept
  )
import Timeline.Convert.UISets.Timelines (getTimelineScoped, addTimelineScoped)
import Timeline.Convert.UISets.Events (getEventScoped, addEventScoped)
import Timeline.Convert.UISets.TimeSpans (getTimeSpanScoped, addTimeSpanScoped)
import Timeline.Convert.UISets.EventsOrTimeSpans (getEventOrTimeSpanScoped)
import Timeline.Convert.Errors (PopulateError(..), SynthesizeError(..))
import Timeline.UI.TimeSpace (TimeSpace(..)) as UI
import Timeline.UI.Timeline (Timeline(..)) as UI
import Timeline.UI.Event (Event(..)) as UI
import Timeline.UI.TimeSpan (TimeSpan(..)) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..), EventOrTimeSpan(..)) as UI
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, write, read) as Ref
import Effect.Class (class MonadEffect, liftEffect)
import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (new, insert, assign, assignExcept, get, subscribeLight) as IxSignalMap
import Unsafe.Coerce (unsafeCoerce)

-- | Sets for all content, indexed by their UUID
newtype UISets
  = UISets
  { timeSpaces :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpace
  , timelines :: IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) UI.Timeline
  , events :: IxSignalMap EventID ( read :: S.READ, write :: S.WRITE ) UI.Event
  , timeSpans :: IxSignalMap TimeSpanID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpan
  , root :: Ref (Maybe TimeSpaceID)
  }

getTimeSpacesMapping :: UISets -> IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpace
getTimeSpacesMapping (UISets { timeSpaces }) = timeSpaces

getTimelinesMapping :: UISets -> IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) UI.Timeline
getTimelinesMapping (UISets { timelines }) = timelines

getEventsMapping :: UISets -> IxSignalMap EventID ( read :: S.READ, write :: S.WRITE ) UI.Event
getEventsMapping (UISets { events }) = events

getTimeSpansMapping :: UISets -> IxSignalMap TimeSpanID ( read :: S.READ, write :: S.WRITE ) UI.TimeSpan
getTimeSpansMapping (UISets { timeSpans }) = timeSpans

getRootRef :: UISets -> Ref (Maybe TimeSpaceID)
getRootRef (UISets { root }) = root

new :: Effect UISets
new = do
  timeSpaces <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  timelines <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  events <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  timeSpans <- IxSignalMap.new { fromString: unsafeCoerce, toString: unsafeCoerce }
  -- can update timelines or siblings
  let
    handleTimeSpacesMappingUpdate :: Tuple TimeSpaceID (MapUpdate UI.TimeSpace) -> Effect Unit
    handleTimeSpacesMappingUpdate (Tuple _ mapUpdate) = case mapUpdate of
      -- Via Insert, you know that all instances will be "new", to the universe of sets
      -- FIXME siblings and timelines should already exist, and if not, they will by the time lookups happen
      MapInsert _ -> pure unit
      -- FIXME find and update timeliens / siblings parent
      MapUpdate { valueNew: UI.TimeSpace { timelines, siblings } } -> pure unit
      -- FIXME delete children
      MapDelete { valueOld: UI.TimeSpace { timelines, siblings } } -> pure unit
  IxSignalMap.subscribeLight "UISets" handleTimeSpacesMappingUpdate timeSpaces
  let
    handleTimelinesMappingUpdate :: Tuple TimelineID (MapUpdate UI.Timeline) -> Effect Unit
    handleTimelinesMappingUpdate (Tuple id mapUpdate) = case mapUpdate of
      -- FIXME children should already exist, and if not, they will by the time lookups happen
      MapInsert { valueNew: UI.Timeline { timeSpace } } -> void (appendTimelineScopedExcept [ "UISets" ] id timeSpace timeSpaces)
      MapUpdate { valueOld: UI.Timeline { children: childrenOld, timeSpace: timeSpaceOld }, valueNew: UI.Timeline { children: childrenNew, timeSpace: timeSpaceNew } } ->
        -- Implements movement of self, FIXME ignores children's parent references
        if timeSpaceOld == timeSpaceNew then
          pure unit
        else do
          void (removeTimelineScopedExcept [ "UISets" ] id timeSpaceOld timeSpaces)
          void (appendTimelineScopedExcept [ "UISets" ] id timeSpaceNew timeSpaces)
      -- removes self from parent
      MapDelete { valueOld: UI.Timeline { timeSpace } } -> void (removeTimelineScopedExcept [ "UISets" ] id timeSpace timeSpaces)
  -- FIXME finish rest of mapping updates
  root <- Ref.new Nothing
  pure
    $ UISets
        { timeSpaces, timelines, events, timeSpans, root }

newtype UISetsM e a
  = UISetsM (ReaderT UISets (ExceptT e Effect) a)

derive newtype instance functorUISetsM :: Functor (UISetsM e)

derive newtype instance applyUISetsM :: Apply (UISetsM e)

derive newtype instance applicativeUISetsM :: Applicative (UISetsM e)

derive newtype instance bindUISetsM :: Bind (UISetsM e)

derive newtype instance monadUISetsM :: Monad (UISetsM e)

derive newtype instance monadEffectUISetsM :: MonadEffect (UISetsM e)

derive newtype instance monadThrowUISetsM :: MonadThrow e (UISetsM e)

derive newtype instance monadAskUISetsM :: MonadAsk UISets (UISetsM e)

runUISetsM :: forall e a. UISetsM e a -> UISets -> Effect (Either e a)
runUISetsM (UISetsM x) s = runExceptT (runReaderT x s)

asUISetsM :: forall e a p q. (UISets -> q) -> (p -> q -> Effect (Either e a)) -> p -> UISetsM e a
asUISetsM fromUISets f p = do
  uiSets <- ask
  let
    q = fromUISets uiSets
  eX <- liftEffect (f p q)
  case eX of
    Left e -> throwError e
    Right x -> pure x

asUISetsM' :: forall e a q. (UISets -> q) -> (q -> Effect (Either e a)) -> UISetsM e a
asUISetsM' fromUISets f = do
  uiSets <- ask
  let
    q = fromUISets uiSets
  eX <- liftEffect (f q)
  case eX of
    Left e -> throwError e
    Right x -> pure x

-- | Includes an already flat time space - doesn't verify constituents
addTimeSpace :: UI.TimeSpace -> UISetsM PopulateError Unit
addTimeSpace = asUISetsM (S.writeOnly <<< getTimeSpacesMapping) addTimeSpaceScoped

-- | Doesn't fail when existing - just re-assigns
addTimeSpaceForce :: UI.TimeSpace -> UISets -> Effect Unit
addTimeSpaceForce x@(UI.TimeSpace { id }) (UISets { timeSpaces }) = addTimeSpaceForceScoped x (S.writeOnly timeSpaces)

-- | Looks for an already flat time space in the sets
getTimeSpace :: TimeSpaceID -> UISetsM SynthesizeError UI.TimeSpace
getTimeSpace = asUISetsM (S.readOnly <<< getTimeSpacesMapping) getTimeSpaceScoped

-- | Includes an already flat timeline - doesn't verify constituents
addTimeline :: UI.Timeline -> UISetsM PopulateError Unit
addTimeline = asUISetsM (S.writeOnly <<< getTimelinesMapping) addTimelineScoped

addTimelineForce :: UI.Timeline -> UISets -> Effect Unit
addTimelineForce x@(UI.Timeline { id }) (UISets { timelines }) = IxSignalMap.assign id x timelines

-- | Looks for an already flat timeline in the sets
getTimeline :: TimelineID -> UISetsM SynthesizeError UI.Timeline
getTimeline = asUISetsM (S.readOnly <<< getTimelinesMapping) getTimelineScoped

-- | Includes an already flat event as a sibling - doesn't verify constituents
addEvent :: UI.Event -> UISetsM PopulateError Unit
addEvent = asUISetsM (S.writeOnly <<< getEventsMapping) addEventScoped

addEventForce :: UI.Event -> UISets -> Effect Unit
addEventForce x@(UI.Event { id }) (UISets { events }) = IxSignalMap.assign id x events

-- | Looks for an already flat event (as a sibling) in the sets
getEvent :: EventID -> UISetsM SynthesizeError UI.Event
getEvent = asUISetsM (S.readOnly <<< getEventsMapping) getEventScoped

-- | Includes an already flat time span as a sibling - doesn't verify constituents
addTimeSpan :: UI.TimeSpan -> UISetsM PopulateError Unit
addTimeSpan = asUISetsM (S.writeOnly <<< getTimeSpansMapping) addTimeSpanScoped

addTimeSpanForce :: UI.TimeSpan -> UISets -> Effect Unit
addTimeSpanForce x@(UI.TimeSpan { id }) (UISets { timeSpans }) = IxSignalMap.assign id x timeSpans

-- | Looks for an already flat time span (as a sibling) in the sets
getTimeSpan :: TimeSpanID -> UISetsM SynthesizeError UI.TimeSpan
getTimeSpan = asUISetsM (S.readOnly <<< getTimeSpansMapping) getTimeSpanScoped

getEventOrTimeSpan :: UI.EventOrTimeSpanPoly EventID TimeSpanID -> UISetsM SynthesizeError UI.EventOrTimeSpan
getEventOrTimeSpan = asUISetsM getEventsAndTimeSpansMappings getEventOrTimeSpanAsTuple
  where
  getEventsAndTimeSpansMappings uiSets = Tuple (S.readOnly (getEventsMapping uiSets)) (S.readOnly (getTimeSpansMapping uiSets))

  getEventOrTimeSpanAsTuple id (Tuple events timeSpans) = getEventOrTimeSpanScoped id events timeSpans

-- | Assigns the root field of a set
setRoot :: TimeSpaceID -> UISets -> Effect Unit
setRoot id (UISets { root }) = Ref.write (Just id) root

getRoot :: UISets -> Effect (Maybe TimeSpaceID)
getRoot (UISets { root }) = Ref.read root
