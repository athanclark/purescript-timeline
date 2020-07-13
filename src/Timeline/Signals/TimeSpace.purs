module Timeline.Signals.TimeSpace where

import Timeline.UI.TimeSpace (TimeSpace)
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.Convert.UISets (getTimeSpaceScoped, addTimeSpaceForceScopedExcept)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array (last) as Array
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE, readOnly, writeOnly) as S
import IxZeta (IxSignal)
import IxZeta (make, setExcept, get, subscribeLight) as IxSignal
import IxZeta.Map (IxSignalMap, MapUpdate(..))
import IxZeta.Map (subscribeLight) as IxSignalMap

-- | Utility function that fetches the "most relevant" time space id, rather than using
-- | the trie series of indicies.
getCurrentTimeSpaceID ::
  { timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect TimeSpaceID
getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef } = do
  timeSpaceIDs <- IxSignal.get timeSpaceSelectedSignal
  case Array.last timeSpaceIDs of
    Just last -> pure last
    Nothing -> do
      mRoot <- Ref.read rootRef
      case mRoot of
        Nothing -> throw "No root TimeSpaceID"
        Just root -> pure root

getCurrentTimeSpace ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect TimeSpace
getCurrentTimeSpace { timeSpaceSelectedSignal, rootRef, timeSpacesMapping } = do
  currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
  eTimeSpace <- getTimeSpaceScoped currentTimeSpaceID timeSpacesMapping
  case eTimeSpace of
    Left e -> throw $ "Cannot getTimeSpaceScoped - error: " <> show e -- FIXME format errors
    Right x -> pure x

-- | Create the "viewed time space" signal on boot, where the initial
-- | time space is synthesized by `UISets`.
newTimeSpaceSignal ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ, write :: S.WRITE ) TimeSpace
  , timeSpaceSelectedSignal :: IxSignal ( read :: S.READ ) (Array TimeSpaceID)
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpace)
newTimeSpaceSignal { timeSpacesMapping, timeSpaceSelectedSignal, rootRef } = do
  currentTimeSpace <-
    getCurrentTimeSpace
      { timeSpacesMapping: S.readOnly timeSpacesMapping
      , timeSpaceSelectedSignal
      , rootRef
      }
  sig <- IxSignal.make currentTimeSpace
  let
    getNewTimeSpace :: Effect Unit
    getNewTimeSpace = do
      newTimeSpace <-
        getCurrentTimeSpace
          { timeSpacesMapping: S.readOnly timeSpacesMapping
          , timeSpaceSelectedSignal
          , rootRef
          }
      IxSignal.setExcept [ "TimeSpaceSignal" ] newTimeSpace sig
  IxSignal.subscribeLight "TimeSpaceSignal" (const getNewTimeSpace) timeSpaceSelectedSignal
  let
    handleMappingUpdate :: Tuple TimeSpaceID (MapUpdate TimeSpace) -> Effect Unit
    handleMappingUpdate (Tuple updatedTimeSpaceID updatedTimeSpace) = do
      currentTimeSpaceID <- getCurrentTimeSpaceID { timeSpaceSelectedSignal, rootRef }
      if currentTimeSpaceID /= updatedTimeSpaceID then
        pure unit
      else case updatedTimeSpace of
        -- FIXME weird predicament (same ID, but not tracking it?) but okay
        MapInsert { valueNew } -> IxSignal.setExcept [ "TimeSpaceSignal" ] valueNew sig
        MapUpdate { valueNew } -> IxSignal.setExcept [ "TimeSpaceSignal" ] valueNew sig
        -- FIXME Uhhhhh.... should I just wait? Or should I navigate to a higher timeSpace? FIXME what if root gets deleted while viewing it?
        MapDelete _ -> pure unit
  IxSignalMap.subscribeLight "TimeSpaceSignal" handleMappingUpdate timeSpacesMapping
  -- Overwrite existing time space in set when viewed updates
  let
    handleViewedUpdate :: TimeSpace -> Effect Unit
    handleViewedUpdate timeSpace = addTimeSpaceForceScopedExcept [ "TimeSpaceSignal" ] timeSpace (S.writeOnly timeSpacesMapping)
  IxSignal.subscribeLight "TimeSpaceSignal" handleViewedUpdate sig
  pure sig
