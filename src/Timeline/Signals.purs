module Timeline.Signals where

-- import Timeline.UI.TimeSpace (TimeSpace (..))
-- import Timeline.UI.TimeSpace.Explore (ExploreTimeSpaces)
-- import Timeline.UI.Timeline (Timeline (..))
-- import Timeline.UI.TimeSpan (TimeSpan)
-- import Timeline.ID.TimeSpace (TimeSpaceID)
-- import Timeline.ID.Timeline (TimelineID)
-- import Timeline.ID.TimeSpan (TimeSpanID)
-- import Timeline.Convert.UISets (getTimeSpaceScoped, getTimelineScoped, addTimeSpaceForceScopedExcept)
-- import Timeline.Convert (synthesizeExploreTimeSpacesScoped)
-- import Prelude
-- import Data.Maybe (Maybe (..))
-- import Data.Tuple (Tuple (..))
-- import Data.Either (Either (..))
-- import Data.Array (last, findIndex, snoc, updateAt, deleteAt, insertAt) as Array
-- import Data.Traversable (traverse)
-- import Effect (Effect)
-- import Effect.Ref (Ref)
-- import Effect.Ref (read) as Ref
-- import Effect.Exception (throw)
-- import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
-- import IxZeta (IxSignal)
-- import IxZeta (make, set, setExcept, get, subscribeLight) as IxSignal
-- import IxZeta.Map (IxSignalMap, MapUpdate (..))
-- import IxZeta.Map (subscribeLight, updateExcept, assignExcept, deleteExcept) as IxSignalMap
-- import IxZeta.Array (IxSignalArray, ArrayUpdate (..))
-- import IxZeta.Array (new, subscribeLight, overwriteExcept, appendExcept, updateExcept, deleteExcept, get) as IxSignalArray
-- FIXME Events and TimeSpans would have `parent :: Either TimeSpaceID TimelineID`?
-- newSiblingsSignal ::
--   { timeSpacesMapping :: IxSignalMap TimeSpaceID (read :: S.READ, write :: S.WRITE) TimeSpace
--   , eventsMapping :: IxSignalMap EventID (read :: S.READ, write :: S.WRITE) Event
--   , timeSpansMapping :: IxSignalMap TimeSpaceID (read :: S.READ, write :: S.WRITE) TimeSpan
--   , timeSpaceSelectedSignal :: IxSignal (read :: S.READ) TimeSpaceID
--   , rootRef :: Ref (Maybe TimeSpaceID)
--   } ->
-- newChildrenSignal ::
--   { timelinesMapping :: IxSignalMap TimelineID (read :: S.READ, write :: S.WRITE) Timeline
--   , eventsMapping :: IxSignalMap EventID (read :: S.READ, write :: S.WRITE) Event
--   , timeSpansMapping :: IxSignalMap TimeSpaceID (read :: S.READ, write :: S.WRITE) TimeSpan
--   , timelineSelectedSignal :: IxSignal (read :: S.READ) (Maybe TimelineID)
--   } ->
-- | Event that triggers a reset of the document
-- FIXME do it for the whole UISets
-- setNewDocumentTimeSpace ::
--   IxSignal ( write :: S.WRITE ) TimeSpace ->
--   Effect Unit
-- setNewDocumentTimeSpace timeSpaceSignal = do
--   timeSpace <- defaultTimeSpace
--   set timeSpace timeSpaceSignal