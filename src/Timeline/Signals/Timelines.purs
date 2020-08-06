module Timeline.Signals.Timelines (newTimelinesSignal) where

import Timeline.UI.TimeSpace (TimeSpace(..))
import Timeline.UI.Timeline (Timeline(..))
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.Convert.UISets.Timelines (getTimelineScoped)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array (findIndex, snoc, deleteAt, insertAt) as Array
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (snoc, deleteAt, insertAt, unsafeTraverse, unsafeMap, toArray) as UniqueArray
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
import IxZeta (IxSignal)
import IxZeta (subscribeLight, get, setExcept) as IxSignal
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (subscribeLight, assignExcept, deleteExcept) as IxSignalMap
import IxZeta.Array (IxSignalArray, ArrayUpdate(..))
import IxZeta.Array (new, subscribeLight, overwriteExcept, appendExcept, updateExcept, deleteExcept, get) as IxSignalArray
import IxZeta.Array.Unique (IxSignalUniqueArray, UniqueArrayUpdate(..))
import IxZeta.Array.Unique (new, subscribeLight, overwriteExcept, appendExcept, updateExcept, deleteExcept, get) as IxSignalUniqueArray

newTimelinesSignal ::
  { timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  } ->
  Effect (IxSignalUniqueArray ( read :: S.READ, write :: S.WRITE ) Timeline)
newTimelinesSignal { timelinesMapping, timeSpaceSignal, timeSpaceSelectedSignal } = do
  sig <-
    makeTimelinesSignal
      { timelinesMapping: S.readOnly timelinesMapping
      , timeSpaceSignal: S.readOnly timeSpaceSignal
      }
  writeOnTimelineMappingUpdate
    { sig
    , timelinesMapping: S.readOnly timelinesMapping
    , timeSpaceSignal: S.readOnly timeSpaceSignal
    }
  writeOnTimeSpaceSelectedUpdate
    { sig: S.writeOnly sig
    , timelinesMapping: S.readOnly timelinesMapping
    , timeSpaceSignal: S.readOnly timeSpaceSignal
    , timeSpaceSelectedSignal
    }
  writeOnSelfUpdate
    { sig: S.readOnly sig
    , timelinesMapping: S.writeOnly timelinesMapping
    , timeSpaceSignal
    }
  pure sig

-- | Create the Timelines signal, and update it When the timespaces mapping updates
makeTimelinesSignal ::
  { timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ ) TimeSpace
  } ->
  Effect (IxSignalUniqueArray ( read :: S.READ, write :: S.WRITE ) Timeline)
makeTimelinesSignal { timelinesMapping, timeSpaceSignal } = do
  initialTimelines <- getLatestTimelines { timelinesMapping, timeSpaceSignal }
  sig <- IxSignalUniqueArray.new initialTimelines
  let
    handleTimeSpaceUpdate :: TimeSpace -> Effect Unit
    handleTimeSpaceUpdate _ = do
      timelines <- getLatestTimelines { timelinesMapping, timeSpaceSignal }
      IxSignalUniqueArray.overwriteExcept [ "TimelinesSignal" ] timelines sig
  IxSignal.subscribeLight "TimelinesSignal" handleTimeSpaceUpdate timeSpaceSignal
  pure sig

-- | Updates the output signal array when the timeline mapping updates.
writeOnTimelineMappingUpdate ::
  { sig :: IxSignalUniqueArray ( read :: S.READ, write :: S.WRITE ) Timeline
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ ) TimeSpace
  } ->
  Effect Unit
writeOnTimelineMappingUpdate { sig, timelinesMapping, timeSpaceSignal } = do
  let
    handleTimelineMappingUpdate :: Tuple TimelineID (MapUpdate Timeline) -> Effect Unit
    handleTimelineMappingUpdate (Tuple timelineID updatedTimeline) = do
      case updatedTimeline of
        MapInsert { valueNew: timeline@(Timeline { timeSpace }) } -> do
          TimeSpace { id: currentTimeSpaceID } <- IxSignal.get timeSpaceSignal
          if timeSpace /= currentTimeSpaceID then
            pure unit
          else do
            _ <- IxSignalUniqueArray.appendExcept [ "TimelinesSignal" ] timeline sig
            pure unit
        -- FIXME will the timeSpacesMapping be updated with the addition to its timeline list?
        -- Should the maps be bound to each other?
        _ -> do
          viewedTimelines <- IxSignalUniqueArray.get sig
          case Array.findIndex (\(Timeline { id }) -> id == timelineID) (UniqueArray.toArray viewedTimelines) of
            Nothing -> pure unit
            Just index -> case updatedTimeline of
              MapInsert _ -> pure unit -- If we already are looking at it, there's nothing to do
              MapUpdate { valueNew } -> do
                succeeded <- IxSignalUniqueArray.updateExcept [ "TimelinesSignal" ] index (const valueNew) sig
                if succeeded then
                  pure unit
                else
                  throw $ "Couldn't update timeline: " <> show valueNew <> ", index: " <> show index
              MapDelete _ -> do
                succeeded <- IxSignalUniqueArray.deleteExcept [ "TimelinesSignal" ] index sig
                if succeeded then
                  pure unit
                else
                  throw $ "Couldn't delete timeline, index: " <> show index
  IxSignalMap.subscribeLight "TimelinesSignal" handleTimelineMappingUpdate timelinesMapping

-- | Gets the latest timelines for the selected signal, when the time space selected signal updates
writeOnTimeSpaceSelectedUpdate ::
  { sig :: IxSignalUniqueArray ( write :: S.WRITE ) Timeline
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  } ->
  Effect Unit
writeOnTimeSpaceSelectedUpdate { sig, timelinesMapping, timeSpaceSignal, timeSpaceSelectedSignal } = do
  -- FIXME make sure this is setDiff when used in dialog
  let
    handleTimeSpaceSelectedUpdate :: Effect Unit
    handleTimeSpaceSelectedUpdate = do
      timelines <- getLatestTimelines { timelinesMapping, timeSpaceSignal }
      IxSignalUniqueArray.overwriteExcept [ "TimelinesSignal" ] timelines sig
  IxSignal.subscribeLight "TimelinesSignal" (const handleTimeSpaceSelectedUpdate) timeSpaceSelectedSignal

-- | Write to the mapping signals when the signal array (being created) updates
writeOnSelfUpdate ::
  { sig :: IxSignalUniqueArray ( read :: S.READ ) Timeline
  , timelinesMapping :: IxSignalMap TimelineID ( write :: S.WRITE ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpace
  } ->
  Effect Unit
writeOnSelfUpdate { sig, timelinesMapping, timeSpaceSignal } = do
  let
    handleSelfUpdate :: UniqueArrayUpdate Timeline -> Effect Unit
    handleSelfUpdate updatedTimeline = case updatedTimeline of
      UniqueArrayAppend { valueNew: valueNew@(Timeline { id }) } -> do
        -- FIXME add to timelinesMapping?
        IxSignalMap.assignExcept [ "TimelinesSignal" ] id valueNew timelinesMapping
        TimeSpace x <- IxSignal.get timeSpaceSignal
        case UniqueArray.snoc x.timelines id of
          Nothing -> throw $ "id already exists: " <> show id <> ", timelines: " <> show x.timelines -- FIXME
          Just xs' -> IxSignal.setExcept [ "TimelinesSignal" ] (TimeSpace x { timelines = xs' }) timeSpaceSignal
      UniqueArrayUpdate { index, valueNew: valueNew@(Timeline { id }) } -> do
        IxSignalMap.assignExcept [ "TimelinesSignal" ] id valueNew timelinesMapping
      -- FIXME No need to update timespace array data, because the id shouldn't have changed
      -- currentTimeSpace@(TimeSpace x) <- getCurrentTimeSpace
      --   { timeSpacesMapping: S.readOnly timeSpacesMapping
      --   , timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
      --   , rootRef
      --   }
      -- case Array.updateAt index id x.timelines of
      --   Nothing -> throw $ "Couldn't update timeline: " <> show currentTimeSpace <> ", index: " <> show index <> ", value: " <> show valueNew
      --   Just timelines' -> do
      --     currentTimeSpaceID <- getCurrentTimeSpaceID {timeSpaceSelectedSignal, rootRef}
      --     IxSignalMap.assignExcept ["TimelinesSignal"] currentTimeSpaceID
      --       (TimeSpace x {timelines = timelines'}) timeSpacesMapping
      UniqueArrayDelete { index, valueOld: Timeline { id } } -> do
        -- ignore success - if it's not there, then we're still okay
        -- FIXME for movement feature, "remove" will be different from "delete"
        _ <- IxSignalMap.deleteExcept [ "TimelinesSignal" ] id timelinesMapping -- FIXME what if the timeline is just moved to a different timespace?
        currentTimeSpace@(TimeSpace x@{ timelines }) <- IxSignal.get timeSpaceSignal
        case UniqueArray.deleteAt index timelines of
          Nothing -> throw $ "Couldn't delete timeline: " <> show currentTimeSpace <> ", index: " <> show index
          Just timelines' -> IxSignal.setExcept [ "TimelinesSignal" ] (TimeSpace x { timelines = timelines' }) timeSpaceSignal
      UniqueArrayMove { indexOld, indexNew, value: value@(Timeline { id }) } -> do
        currentTimeSpace@(TimeSpace x@{ timelines }) <- IxSignal.get timeSpaceSignal
        case UniqueArray.deleteAt indexOld timelines >>= UniqueArray.insertAt indexNew id of
          Nothing -> throw $ "Couldn't move timeline: " <> show currentTimeSpace <> ", indexOld: " <> show indexOld <> ", indexNew: " <> show indexNew
          Just timelines' -> IxSignal.setExcept [ "TimelinesSignal" ] (TimeSpace x { timelines = timelines' }) timeSpaceSignal
      UniqueArrayOverwrite { values } -> do
        -- TODO no need to add or update information in timelinesMapping? Because this only happens when loading from a timespace?
        TimeSpace x <- IxSignal.get timeSpaceSignal
        IxSignal.setExcept [ "TimelinesSignal" ] (TimeSpace x { timelines = UniqueArray.unsafeMap (\(Timeline { id }) -> id) values }) timeSpaceSignal
  IxSignalUniqueArray.subscribeLight "TimelinesSignal" handleSelfUpdate sig

-- | Fetch the latest timelines for the currently selected time space
getLatestTimelines ::
  { timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) Timeline
  , timeSpaceSignal :: IxSignal ( read :: S.READ ) TimeSpace
  } ->
  Effect (UniqueArray Timeline)
getLatestTimelines { timelinesMapping, timeSpaceSignal } = do
  TimeSpace { timelines } <- IxSignal.get timeSpaceSignal
  let
    getTimeline' :: TimelineID -> Effect Timeline
    getTimeline' id = do
      eTimeline <- getTimelineScoped id timelinesMapping
      case eTimeline of
        Left e -> throw $ "Couldn't get timeline: " <> show e
        Right x -> pure x
  UniqueArray.unsafeTraverse getTimeline' timelines
