module Timeline.Convert.UISets.TimeSpaces where

import Timeline.Convert.Errors (PopulateError(TimeSpaceExists), SynthesizeError(TimeSpaceDoesntExist))
import Timeline.UI.TimeSpace (TimeSpace(..))
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (snoc, filter) as Array
import Data.Array.Unique (UniqueArray)
import Data.Array.Unique (toArray, unsafeFromArray, snoc) as UniqueArray
import Effect (Effect)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE) as S
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (insert, get, assign, assignExcept) as IxSignalMap

addTimeSpaceScoped :: TimeSpace -> IxSignalMap TimeSpaceID ( write :: S.WRITE ) TimeSpace -> Effect (Either PopulateError Unit)
addTimeSpaceScoped x@(TimeSpace { id }) timeSpaces = do
  succeeded <- IxSignalMap.insert id x timeSpaces
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimeSpaceExists x)

addTimeSpaceForceScoped :: TimeSpace -> IxSignalMap TimeSpaceID ( write :: S.WRITE ) TimeSpace -> Effect Unit
addTimeSpaceForceScoped x@(TimeSpace { id }) timeSpaces = IxSignalMap.assign id x timeSpaces

addTimeSpaceForceScopedExcept :: Array String -> TimeSpace -> IxSignalMap TimeSpaceID ( write :: S.WRITE ) TimeSpace -> Effect Unit
addTimeSpaceForceScopedExcept indicies x@(TimeSpace { id }) timeSpaces = IxSignalMap.assignExcept indicies id x timeSpaces

-- FIXME check duplicates?
appendTimelineScopedExcept :: Array String -> TimelineID -> TimeSpaceID -> IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace -> Effect (Maybe TimeSpace)
appendTimelineScopedExcept indicies timelineID timeSpaceID timeSpaces = do
  mX <- IxSignalMap.get timeSpaceID timeSpaces
  case mX of
    Nothing -> pure Nothing
    Just (TimeSpace x@{ timelines }) -> do
      case UniqueArray.snoc timelines timelineID of
        Nothing -> throw $ "timeline Id already exists: " <> show timelineID
        Just timelines' -> do
          let
            new :: TimeSpace
            new = TimeSpace $ x { timelines = timelines' }
          IxSignalMap.assignExcept indicies timeSpaceID new timeSpaces
          pure (Just new)

removeTimelineScopedExcept :: Array String -> TimelineID -> TimeSpaceID -> IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace -> Effect (Maybe TimeSpace)
removeTimelineScopedExcept indicies timelineID timeSpaceID timeSpaces = do
  mX <- IxSignalMap.get timeSpaceID timeSpaces
  case mX of
    Nothing -> pure Nothing
    Just (TimeSpace x@{ timelines }) -> do
      let
        timelines' = UniqueArray.toArray timelines

        timelines'' = Array.filter (_ /= timelineID) timelines'

        new = TimeSpace x { timelines = UniqueArray.unsafeFromArray timelines'' }
      IxSignalMap.assignExcept indicies timeSpaceID new timeSpaces
      pure (Just new)

getTimeSpaceScoped :: TimeSpaceID -> IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace -> Effect (Either SynthesizeError TimeSpace)
getTimeSpaceScoped id timeSpaces = do
  mTimeSpace <- IxSignalMap.get id timeSpaces
  pure
    $ case mTimeSpace of
        Nothing -> Left (TimeSpaceDoesntExist id)
        Just timeSpace -> Right timeSpace
