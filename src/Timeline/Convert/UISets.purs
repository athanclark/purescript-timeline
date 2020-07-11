module Timeline.Convert.UISets where

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
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (new, set, set', get) as IxSignalMap
import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
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

-- derive newtype instance monadErrorUISetsM :: MonadError e (UISetsM e)
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

addTimeSpaceScoped :: UI.TimeSpace -> IxSignalMap TimeSpaceID ( write :: S.WRITE ) UI.TimeSpace -> Effect (Either PopulateError Unit)
addTimeSpaceScoped x@(UI.TimeSpace { id }) timeSpaces = do
  succeeded <- IxSignalMap.set' id x timeSpaces
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimeSpaceExists x)

-- | Doesn't fail when existing - just re-assigns
addTimeSpaceForce :: UI.TimeSpace -> UISets -> Effect Unit
addTimeSpaceForce x@(UI.TimeSpace { id }) (UISets { timeSpaces }) = IxSignalMap.set id x timeSpaces

-- | Looks for an already flat time space in the sets
getTimeSpace :: TimeSpaceID -> UISetsM SynthesizeError UI.TimeSpace
getTimeSpace = asUISetsM (S.readOnly <<< getTimeSpacesMapping) getTimeSpaceScoped

getTimeSpaceScoped :: TimeSpaceID -> IxSignalMap TimeSpaceID ( read :: S.READ ) UI.TimeSpace -> Effect (Either SynthesizeError UI.TimeSpace)
getTimeSpaceScoped id timeSpaces = do
  mTimeSpace <- IxSignalMap.get id timeSpaces
  pure
    $ case mTimeSpace of
        Nothing -> Left (TimeSpaceDoesntExist id)
        Just timeSpace -> Right timeSpace

-- | Includes an already flat timeline - doesn't verify constituents
addTimeline :: UI.Timeline -> UISetsM PopulateError Unit
addTimeline = asUISetsM (S.writeOnly <<< getTimelinesMapping) addTimelineScoped

addTimelineScoped :: UI.Timeline -> IxSignalMap TimelineID ( write :: S.WRITE ) UI.Timeline -> Effect (Either PopulateError Unit)
addTimelineScoped x@(UI.Timeline { id }) timelines = do
  succeeded <- IxSignalMap.set' id x timelines
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimelineExists x)

addTimelineForce :: UI.Timeline -> UISets -> Effect Unit
addTimelineForce x@(UI.Timeline { id }) (UISets { timelines }) = IxSignalMap.set id x timelines

-- | Looks for an already flat timeline in the sets
getTimeline :: TimelineID -> UISetsM SynthesizeError UI.Timeline
getTimeline = asUISetsM (S.readOnly <<< getTimelinesMapping) getTimelineScoped

getTimelineScoped :: TimelineID -> IxSignalMap TimelineID ( read :: S.READ ) UI.Timeline -> Effect (Either SynthesizeError UI.Timeline)
getTimelineScoped id timelines = do
  mTimeline <- IxSignalMap.get id timelines
  pure
    $ case mTimeline of
        Nothing -> Left (TimelineDoesntExist id)
        Just timeline -> Right timeline

-- | Includes an already flat event as a sibling - doesn't verify constituents
addEvent :: UI.Event -> UISetsM PopulateError Unit
addEvent = asUISetsM (S.writeOnly <<< getEventsMapping) addEventScoped

addEventScoped :: UI.Event -> IxSignalMap EventID ( write :: S.WRITE ) UI.Event -> Effect (Either PopulateError Unit)
addEventScoped x@(UI.Event { id }) events = do
  succeeded <- IxSignalMap.set' id x events
  pure
    $ if succeeded then
        Right unit
      else
        Left (EventExists x)

addEventForce :: UI.Event -> UISets -> Effect Unit
addEventForce x@(UI.Event { id }) (UISets { events }) = IxSignalMap.set id x events

-- | Looks for an already flat event (as a sibling) in the sets
getEvent :: EventID -> UISetsM SynthesizeError UI.Event
getEvent = asUISetsM (S.readOnly <<< getEventsMapping) getEventScoped

getEventScoped :: EventID -> IxSignalMap EventID ( read :: S.READ ) UI.Event -> Effect (Either SynthesizeError UI.Event)
getEventScoped id events = do
  mEvent <- IxSignalMap.get id events
  pure
    $ case mEvent of
        Nothing -> Left (EventDoesntExist id)
        Just event -> Right event

-- | Includes an already flat time span as a sibling - doesn't verify constituents
addTimeSpan :: UI.TimeSpan -> UISetsM PopulateError Unit
addTimeSpan = asUISetsM (S.writeOnly <<< getTimeSpansMapping) addTimeSpanScoped

addTimeSpanScoped :: UI.TimeSpan -> IxSignalMap TimeSpanID ( write :: S.WRITE ) UI.TimeSpan -> Effect (Either PopulateError Unit)
addTimeSpanScoped x@(UI.TimeSpan { id }) timeSpans = do
  succeeded <- IxSignalMap.set' id x timeSpans
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimeSpanExists x)

addTimeSpanForce :: UI.TimeSpan -> UISets -> Effect Unit
addTimeSpanForce x@(UI.TimeSpan { id }) (UISets { timeSpans }) = IxSignalMap.set id x timeSpans

-- | Looks for an already flat time span (as a sibling) in the sets
getTimeSpan :: TimeSpanID -> UISetsM SynthesizeError UI.TimeSpan
getTimeSpan = asUISetsM (S.readOnly <<< getTimeSpansMapping) getTimeSpanScoped

getTimeSpanScoped :: TimeSpanID -> IxSignalMap TimeSpanID ( read :: S.READ ) UI.TimeSpan -> Effect (Either SynthesizeError UI.TimeSpan)
getTimeSpanScoped id timeSpans = do
  mTimeSpan <- IxSignalMap.get id timeSpans
  pure
    $ case mTimeSpan of
        Nothing -> Left (TimeSpanDoesntExist id)
        Just timeSpan -> Right timeSpan

getEventOrTimeSpan :: UI.EventOrTimeSpanPoly EventID TimeSpanID -> UISetsM SynthesizeError UI.EventOrTimeSpan
getEventOrTimeSpan = asUISetsM getEventsAndTimeSpansMappings getEventOrTimeSpanAsTuple
  where
  getEventsAndTimeSpansMappings uiSets = Tuple (S.readOnly (getEventsMapping uiSets)) (S.readOnly (getTimeSpansMapping uiSets))

  getEventOrTimeSpanAsTuple id (Tuple events timeSpans) = getEventOrTimeSpanScoped id events timeSpans

getEventOrTimeSpanScoped ::
  UI.EventOrTimeSpanPoly EventID TimeSpanID ->
  IxSignalMap EventID ( read :: S.READ ) UI.Event ->
  IxSignalMap TimeSpanID ( read :: S.READ ) UI.TimeSpan ->
  Effect (Either SynthesizeError UI.EventOrTimeSpan)
getEventOrTimeSpanScoped (UI.EventOrTimeSpanPoly eOrTs) events timeSpans = case eOrTs of
  Left e -> map (UI.EventOrTimeSpan <<< Left) <$> getEventScoped e events
  Right ts -> map (UI.EventOrTimeSpan <<< Right) <$> getTimeSpanScoped ts timeSpans

-- | Assigns the root field of a set
setRoot :: TimeSpaceID -> UISets -> Effect Unit
setRoot id (UISets { root }) = Ref.write (Just id) root

getRoot :: UISets -> Effect (Maybe TimeSpaceID)
getRoot (UISets { root }) = Ref.read root
