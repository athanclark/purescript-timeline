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
import Zeta.Types (READ, WRITE) as S
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

-- | Includes an already flat time space - doesn't verify constituents
addTimeSpace :: UI.TimeSpace -> UISetsM PopulateError Unit
addTimeSpace x@(UI.TimeSpace { id }) = do
  UISets { timeSpaces } <- ask
  succeeded <- liftEffect (IxSignalMap.set' id x timeSpaces)
  if succeeded then
    pure unit
  else
    throwError (TimeSpaceExists x)

-- | Doesn't fail when existing - just re-assigns
addTimeSpace' :: UI.TimeSpace -> UISets -> Effect Unit
addTimeSpace' x@(UI.TimeSpace { id }) (UISets { timeSpaces }) = IxSignalMap.set id x timeSpaces

-- | Looks for an already flat time space in the sets
getTimeSpace :: TimeSpaceID -> UISetsM SynthesizeError UI.TimeSpace
getTimeSpace id = do
  UISets { timeSpaces } <- ask
  mTimeSpace <- liftEffect (IxSignalMap.get id timeSpaces)
  case mTimeSpace of
    Nothing -> throwError (TimeSpaceDoesntExist id)
    Just timeSpace -> pure timeSpace

-- | Includes an already flat timeline - doesn't verify constituents
addTimeline :: UI.Timeline -> UISetsM PopulateError Unit
addTimeline x@(UI.Timeline { id }) = do
  UISets { timelines } <- ask
  succeeded <- liftEffect (IxSignalMap.set' id x timelines)
  if succeeded then
    pure unit
  else
    throwError (TimelineExists x)

addTimeline' :: UI.Timeline -> UISets -> Effect Unit
addTimeline' x@(UI.Timeline { id }) (UISets { timelines }) = IxSignalMap.set id x timelines

-- | Looks for an already flat timeline in the sets
getTimeline :: TimelineID -> UISetsM SynthesizeError UI.Timeline
getTimeline id = do
  UISets { timelines } <- ask
  mTimeline <- liftEffect (IxSignalMap.get id timelines)
  case mTimeline of
    Nothing -> throwError (TimelineDoesntExist id)
    Just timeline -> pure timeline

-- | Includes an already flat event as a sibling - doesn't verify constituents
addEvent :: UI.Event -> UISetsM PopulateError Unit
addEvent x@(UI.Event { id }) = do
  UISets { events } <- ask
  succeeded <- liftEffect (IxSignalMap.set' id x events)
  if succeeded then
    pure unit
  else
    throwError (EventExists x)

addEvent' :: UI.Event -> UISets -> Effect Unit
addEvent' x@(UI.Event { id }) (UISets { events }) = IxSignalMap.set id x events

-- | Looks for an already flat event (as a sibling) in the sets
getEvent :: EventID -> UISetsM SynthesizeError UI.Event
getEvent id = do
  UISets { events } <- ask
  mEvent <- liftEffect (IxSignalMap.get id events)
  case mEvent of
    Nothing -> throwError (EventDoesntExist id)
    Just event -> pure event

-- | Includes an already flat time span as a sibling - doesn't verify constituents
addTimeSpan :: UI.TimeSpan -> UISetsM PopulateError Unit
addTimeSpan x@(UI.TimeSpan { id }) = do
  UISets { timeSpans } <- ask
  succeeded <- liftEffect (IxSignalMap.set' id x timeSpans)
  if succeeded then
    pure unit
  else
    throwError (TimeSpanExists x)

addTimeSpan' :: UI.TimeSpan -> UISets -> Effect Unit
addTimeSpan' x@(UI.TimeSpan { id }) (UISets { timeSpans }) = IxSignalMap.set id x timeSpans

-- | Looks for an already flat time span (as a sibling) in the sets
getTimeSpan :: TimeSpanID -> UISetsM SynthesizeError UI.TimeSpan
getTimeSpan id = do
  UISets { timeSpans } <- ask
  mTimeSpan <- liftEffect (IxSignalMap.get id timeSpans)
  case mTimeSpan of
    Nothing -> throwError (TimeSpanDoesntExist id)
    Just timeSpan -> pure timeSpan

getEventOrTimeSpan :: UI.EventOrTimeSpanPoly EventID TimeSpanID -> UISetsM SynthesizeError UI.EventOrTimeSpan
getEventOrTimeSpan (UI.EventOrTimeSpanPoly eOrTs) = case eOrTs of
  Left e -> UI.EventOrTimeSpan <<< Left <$> getEvent e
  Right ts -> UI.EventOrTimeSpan <<< Right <$> getTimeSpan ts

-- | Assigns the root field of a set
setRoot :: TimeSpaceID -> UISets -> Effect Unit
setRoot id (UISets { root }) = Ref.write (Just id) root

getRoot :: UISets -> Effect (Maybe TimeSpaceID)
getRoot (UISets { root }) = Ref.read root
