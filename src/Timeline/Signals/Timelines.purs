module Timeline.Signals.Timelines where

import Timeline.Signals.TimeSpace (getCurrentTimeSpace, getCurrentTimeSpaceID)
import Timeline.UI.TimeSpace (TimeSpace(..))
import Timeline.UI.Timeline (Timeline(..))
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.Convert.UISets (getTimelineScoped)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array (findIndex, snoc, deleteAt, insertAt) as Array
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE, readOnly) as S
import IxZeta (IxSignal)
import IxZeta (subscribeLight) as IxSignal
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (subscribeLight, updateExcept, assignExcept, deleteExcept) as IxSignalMap
import IxZeta.Array (IxSignalArray, ArrayUpdate(..))
import IxZeta.Array (new, subscribeLight, overwriteExcept, appendExcept, updateExcept, deleteExcept, get) as IxSignalArray

newTimelinesSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ, write :: S.WRITE ) Timeline
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignalArray ( read :: S.READ, write :: S.WRITE ) Timeline)
newTimelinesSignal { timeSpacesMapping, timelinesMapping, timeSpaceSelectedSignal, rootRef } = do
  initialTimelines <- getLatestTimelines
  sig <- IxSignalArray.new initialTimelines
  let
    handleTimeSpacesMappingUpdate :: Tuple TimeSpaceID (MapUpdate TimeSpace) -> Effect Unit
    handleTimeSpacesMappingUpdate (Tuple timeSpaceID updatedTimeSpace) = do
      currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
      if timeSpaceID /= currentTimeSpaceID then
        pure unit
      else case updatedTimeSpace of
        MapInsert _ -> pure unit
        MapDelete _ -> pure unit -- what should I do if it's deleted while I'm viewing?
        MapUpdate { valueOld: TimeSpace { timelines: oldTimelines }, valueNew: TimeSpace { timelines: newTimelines } }
          | oldTimelines == newTimelines -> pure unit
          | otherwise -> do
            timelines <- getLatestTimelines
            IxSignalArray.overwriteExcept [ "TimelinesSignal" ] timelines sig
  IxSignalMap.subscribeLight "TimelinesSignal" handleTimeSpacesMappingUpdate timeSpacesMapping
  let
    handleTimelineMappingUpdate :: Tuple TimelineID (MapUpdate Timeline) -> Effect Unit
    handleTimelineMappingUpdate (Tuple timelineID updatedTimeline) = do
      case updatedTimeline of
        MapInsert { valueNew: timeline@(Timeline { timeSpace }) } -> do -- FIXME make field
          currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
          if timeSpace /= currentTimeSpaceID then
            pure unit
          else
            IxSignalArray.appendExcept [ "TimelinesSignal" ] timeline sig
        _ -> do
          viewedTimelines <- IxSignalArray.get sig
          case Array.findIndex (\(Timeline { id }) -> id == timelineID) viewedTimelines of
            Nothing -> pure unit
            Just index -> case updatedTimeline of
              MapInsert _ -> pure unit -- If we already are looking at it, there's nothing to do
              MapUpdate { valueNew } -> do
                succeeded <- IxSignalArray.updateExcept [ "TimelinesSignal" ] index (const valueNew) sig
                if succeeded then
                  pure unit
                else
                  throw $ "Couldn't update timeline: " <> show valueNew <> ", index: " <> show index
              MapDelete _ -> do
                succeeded <- IxSignalArray.deleteExcept [ "TimelinesSignal" ] index sig
                if succeeded then
                  pure unit
                else
                  throw $ "Couldn't delete timeline, index: " <> show index
  IxSignalMap.subscribeLight "TimelinesSignal" handleTimelineMappingUpdate timelinesMapping
  -- FIXME make sure this is setDiff when used in dialog
  let
    handleTimeSpaceSelectedUpdate :: Effect Unit
    handleTimeSpaceSelectedUpdate = do
      timelines <- getLatestTimelines
      IxSignalArray.overwriteExcept [ "TimelinesSignal" ] timelines sig
  IxSignal.subscribeLight "TimelinesSignal" (const handleTimeSpaceSelectedUpdate) timeSpaceSelectedSignal
  let
    handleSelfUpdate :: ArrayUpdate Timeline -> Effect Unit
    handleSelfUpdate updatedTimeline = case updatedTimeline of
      ArrayAppend { valueNew: valueNew@(Timeline { id }) } -> do
        -- FIXME add to timelinesMapping?
        IxSignalMap.assignExcept [ "TimeSpacesSignal" ] id valueNew timelinesMapping
        currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
        succeeded <-
          IxSignalMap.updateExcept [ "TimeSpacesSignal" ] currentTimeSpaceID
            (\(TimeSpace x) -> TimeSpace x { timelines = Array.snoc x.timelines id })
            timeSpacesMapping
        if succeeded then
          pure unit
        else
          throw "Couldn't update via append"
      ArrayUpdate { index, valueNew: valueNew@(Timeline { id }) } -> do
        IxSignalMap.assignExcept [ "TimeSpacesSignal" ] id valueNew timelinesMapping
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
      --     IxSignalMap.assignExcept ["TimeSpacesSignal"] currentTimeSpaceID
      --       (TimeSpace x {timelines = timelines'}) timeSpacesMapping
      ArrayDelete { index, valueOld: Timeline { id } } -> do
        -- ignore success - if it's not there, then we're still okay
        _ <- IxSignalMap.deleteExcept [ "TimeSpacesSignal" ] id timelinesMapping -- FIXME what if the timeline is just moved to a different timespace?
        currentTimeSpace@(TimeSpace x) <-
          getCurrentTimeSpace
            { timeSpacesMapping: S.readOnly timeSpacesMapping
            , timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
            , rootRef
            }
        case Array.deleteAt index x.timelines of
          Nothing -> throw $ "Couldn't delete timeline: " <> show currentTimeSpace <> ", index: " <> show index
          Just timelines' -> do
            currentTimeSpaceID <-
              getCurrentTimeSpaceID
                { timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
                , rootRef
                }
            IxSignalMap.assignExcept [ "TimeSpacesSignal" ] currentTimeSpaceID
              (TimeSpace x { timelines = timelines' })
              timeSpacesMapping
      ArrayMove { indexOld, indexNew, value: value@(Timeline { id }) } -> do
        currentTimeSpace@(TimeSpace x) <-
          getCurrentTimeSpace
            { timeSpacesMapping: S.readOnly timeSpacesMapping
            , timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
            , rootRef
            }
        case Array.deleteAt indexOld x.timelines >>= Array.insertAt indexNew id of
          Nothing -> throw $ "Couldn't move timeline: " <> show currentTimeSpace <> ", indexOld: " <> show indexOld <> ", indexNew: " <> show indexNew
          Just timelines' -> do
            currentTimeSpaceID <-
              getCurrentTimeSpaceID
                { timeSpaceSelectedSignal: timeSpaceSelectedSignal
                , rootRef
                }
            IxSignalMap.assignExcept [ "TimeSpacesSignal" ] currentTimeSpaceID
              (TimeSpace x { timelines = timelines' })
              timeSpacesMapping
      ArrayOverwrite { values } -> do
        -- TODO no need to add or update information in timelinesMapping? Because this only happens when loading from a timespace?
        currentTimeSpaceID <-
          getCurrentTimeSpaceID
            { timeSpaceSelectedSignal: S.readOnly timeSpaceSelectedSignal
            , rootRef
            }
        succeeded <-
          IxSignalMap.updateExcept [ "TimeSpacesSignal" ] currentTimeSpaceID
            (\(TimeSpace x) -> TimeSpace x { timelines = map (\(Timeline { id }) -> id) values })
            timeSpacesMapping
        if succeeded then
          pure unit
        else
          throw "Couldn't update via overwrite"
  IxSignalArray.subscribeLight "TimeSpacesSignal" handleSelfUpdate sig
  pure sig
  where
  getLatestTimelines :: Effect (Array Timeline)
  getLatestTimelines = do
    TimeSpace { timelines } <-
      getCurrentTimeSpace
        { timeSpacesMapping: S.readOnly timeSpacesMapping
        , timeSpaceSelectedSignal
        , rootRef
        }
    let
      getTimeline' :: TimelineID -> Effect Timeline
      getTimeline' id = do
        eTimeline <- getTimelineScoped id (S.readOnly timelinesMapping)
        case eTimeline of
          Left e -> throw $ "Couldn't get timeline: " <> show e
          Right x -> pure x
    traverse getTimeline' timelines
