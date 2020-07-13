module Timeline.Signals.TimeSpace.Explore where

import Timeline.UI.TimeSpace (TimeSpace)
import Timeline.UI.TimeSpace.Explore (ExploreTimeSpaces)
import Timeline.UI.TimeSpan (TimeSpan)
import Timeline.UI.Timeline (Timeline)
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.Convert (synthesizeExploreTimeSpacesScoped)
import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Exception (throw)
import Zeta.Types (READ, readOnly) as S
import IxZeta (IxSignal)
import IxZeta (make, set) as IxSignal
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (subscribeLight) as IxSignalMap

-- | Returns a read-only signal, because nobody should be writing to this signal -
-- | it only changes when constituents change.
newExploreTimeSpacesSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpansMapping :: IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignal ( read :: S.READ ) ExploreTimeSpaces)
newExploreTimeSpacesSignal { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef } = do
  initialExplore <- getLatestExploreTimeSpaces
  sig <- IxSignal.make initialExplore
  let
    updateExploreTimeSpaces = do
      newExplore <- getLatestExploreTimeSpaces
      IxSignal.set newExplore sig
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timeSpacesMapping
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timelinesMapping
  IxSignalMap.subscribeLight "ExploreTimeSpacesSignal" (const updateExploreTimeSpaces) timeSpansMapping
  pure (S.readOnly sig)
  where
  getLatestExploreTimeSpaces :: Effect ExploreTimeSpaces
  getLatestExploreTimeSpaces = do
    eExplore <- synthesizeExploreTimeSpacesScoped { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef }
    case eExplore of
      Left e -> throw $ "Couldn't synthesize explore time space: " <> show e
      Right x -> pure x
