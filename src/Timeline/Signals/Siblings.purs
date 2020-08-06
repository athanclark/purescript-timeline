module Timeline.Signals.Siblings where

import Timeline.Signals.TimeSpace (getCurrentTimeSpace, getCurrentTimeSpaceID)
import Timeline.UI.TimeSpace (TimeSpace(..))
import Timeline.UI.Event (Event)
import Timeline.UI.TimeSpan (TimeSpan)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly, EventOrTimeSpan)
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.Convert.UISets.EventsOrTimeSpans (getEventOrTimeSpanScoped)
import Prelude
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (unsafeTraverse) as UniqueArray
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE, readOnly) as S
import IxZeta (IxSignal)
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (subscribeLight, updateExcept, assignExcept, deleteExcept) as IxSignalMap
import IxZeta.Array.Unique (IxSignalUniqueArray)
import IxZeta.Array.Unique (new, overwriteExcept) as IxSignalUniqueArray

newSiblingsSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace
  , eventsMapping :: IxSignalMap EventID ( read :: S.READ, write :: S.WRITE ) Event
  , timeSpansMapping :: IxSignalMap TimeSpanID ( read :: S.READ, write :: S.WRITE ) TimeSpan
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignalUniqueArray ( read :: S.READ, write :: S.WRITE ) EventOrTimeSpan)
newSiblingsSignal { timeSpacesMapping, eventsMapping, timeSpansMapping, timeSpaceSelectedSignal, rootRef } = do
  -- When the timespaces mapping updates
  let
    getLatestSiblings :: Effect (UniqueArray EventOrTimeSpan)
    getLatestSiblings = do
      TimeSpace { siblings } <-
        getCurrentTimeSpace
          { timeSpacesMapping: S.readOnly timeSpacesMapping
          , timeSpaceSelectedSignal
          , rootRef
          }
      let
        getSibling' ::
          EventOrTimeSpanPoly EventID TimeSpanID ->
          Effect EventOrTimeSpan
        getSibling' id = do
          eEorTs <-
            getEventOrTimeSpanScoped id
              (S.readOnly eventsMapping)
              (S.readOnly timeSpansMapping)
          case eEorTs of
            Left e -> throw $ "Couldn't get event or time span: " <> show e
            Right x -> pure x
      UniqueArray.unsafeTraverse getSibling' siblings
  initialSiblings <- getLatestSiblings
  sig <- IxSignalUniqueArray.new initialSiblings
  let
    handleTimeSpacesMappingUpdate :: Tuple TimeSpaceID (MapUpdate TimeSpace) -> Effect Unit
    handleTimeSpacesMappingUpdate (Tuple timeSpaceID updatedTimeSpace) = do
      currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
      if timeSpaceID /= currentTimeSpaceID then
        pure unit
      else case updatedTimeSpace of
        MapInsert _ -> pure unit
        MapDelete _ -> pure unit -- what should I do if it's deleted while I'm viewing?
        MapUpdate { valueOld: TimeSpace { siblings: oldSiblings }, valueNew: TimeSpace { siblings: newSiblings } }
          | oldSiblings == newSiblings -> pure unit
          | otherwise -> do
            siblings <- getLatestSiblings
            IxSignalUniqueArray.overwriteExcept [ "SiblingsSignal" ] siblings sig
  IxSignalMap.subscribeLight "SiblingsSignal" handleTimeSpacesMappingUpdate timeSpacesMapping
  pure sig
