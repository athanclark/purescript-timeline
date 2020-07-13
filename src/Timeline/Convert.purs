module Timeline.Convert where

import Timeline.Convert.UISets
  ( UISets
  , getTimeSpacesMapping
  , getTimelinesMapping
  , getTimeSpansMapping
  , getRootRef
  , UISetsM
  , runUISetsM
  , asUISetsM'
  , addTimeSpace
  , addTimeline
  , addEvent
  , addTimeSpan
  , setRoot
  , getTimeSpace
  , getTimeSpaceScoped
  , getTimeline
  , getTimelineScoped
  , getEvent
  , getTimeSpan
  , getTimeSpanScoped
  , getRoot
  , new
  )
import Timeline.Convert.Errors (PopulateError(..), SynthesizeError(..))
import Timeline.Convert.TimeScale (dataToUi, uiToData) as ConvertTimeScale
import Timeline.Convert.Event (dataToUi, uiToData) as ConvertEvent
import Timeline.UI.TimeSpace (TimeSpace(..)) as UI
import Timeline.UI.TimeSpace.Explore (ExploreTimeSpaces(..)) as UI
import Timeline.UI.TimeSpace.TimeScale (TimeScale(..)) as UI
import Timeline.UI.Timeline (Timeline(..)) as UI
import Timeline.UI.TimeSpan (TimeSpan(..)) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..)) as UI
import Timeline.Data
  ( TimeSpaceDecided(..)
  , TimeSpace(..)
  , Timeline(..)
  , EventOrTimeSpan(..)
  , TimeSpan(..)
  , Event(..)
  , getIdTimeSpaceDecided
  )
  as Data
import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.Time.Unit (DecidedUnit(..))
import Timeline.Time.Value (DecidedValue(..))
import Timeline.Time.Span (makeDecidedSpan, unmakeDecidedSpan)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array (snoc, foldM, mapMaybe, concatMap) as Array
import Data.Traversable (traverse, sequence)
import Control.Monad.Error.Class (throwError)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import Zeta.Types (READ, readOnly) as S
import IxZeta.Map (IxSignalMap)
import Partial.Unsafe (unsafePartial) -- FIXME

-- | Translates a recursive `TimeSpaceDecided` into a flat `UISets`.
populate :: Data.TimeSpaceDecided -> Effect (Either PopulateError UISets)
populate x = do
  sets <- new
  setRoot (Data.getIdTimeSpaceDecided x) sets
  map (const sets) <$> runUISetsM (populateTimeSpaceDecided x) sets

-- | Recursively add a `TimeSpaceDecided` to the sets
populateTimeSpaceDecided :: Data.TimeSpaceDecided -> UISetsM PopulateError Unit
populateTimeSpaceDecided x = case x of
  Data.TimeSpaceNumber timeSpace -> populateTimeSpace DecidedUnitNumber (map DecidedValueNumber timeSpace)

-- | Recursively add a `TimeSpace` to the sets
populateTimeSpace :: DecidedUnit -> Data.TimeSpace DecidedValue -> UISetsM PopulateError Unit
populateTimeSpace decidedUnit ( Data.TimeSpace
    { timeScale
  , title
  , description
  , timelines
  , siblings
  , id
  }
) = do
  -- actually fold over the timelines
  timelineIds <- Array.foldM populateTimelines [] timelines
  -- actually fold over the siblings
  siblingIds <- Array.foldM populateChildrenOrSiblings [] siblings
  -- convert the time scale
  timeScale' <- case ConvertTimeScale.dataToUi decidedUnit timeScale of
    Nothing -> throwError (ConvertTimeScaleFailed { decidedUnit, timeScale })
    Just x -> pure x
  -- create the new time space
  let
    timeSpace' :: UI.TimeSpace
    timeSpace' =
      UI.TimeSpace
        { title
        , description
        , timeScale: timeScale'
        , siblings: siblingIds
        , timelines: timelineIds
        , id
        }
  -- finally add the new time space to the set
  addTimeSpace timeSpace'
  where
  -- folds through the timelines, gathering ids as they're added to the sets
  populateTimelines ::
    Array TimelineID ->
    Data.Timeline DecidedValue ->
    UISetsM PopulateError (Array TimelineID)
  populateTimelines ids timeline@(Data.Timeline { id: id' }) = do
    populateTimeline id timeline -- id here references time space id
    pure (Array.snoc ids id')

-- | Recursively add a `Timeline` to the sets
populateTimeline :: TimeSpaceID -> Data.Timeline DecidedValue -> UISetsM PopulateError Unit
populateTimeline timeSpace (Data.Timeline { children, name, description, id }) = do
  -- actually fold over the siblings
  childrenIds <- Array.foldM populateChildrenOrSiblings [] children
  let
    timeline' :: UI.Timeline
    timeline' =
      UI.Timeline
        { name
        , description
        , children: childrenIds
        , id
        , timeSpace
        }
  addTimeline timeline'

-- | folds through the children, gathering ids as they're added to the sets
populateChildrenOrSiblings ::
  Array (UI.EventOrTimeSpanPoly EventID TimeSpanID) ->
  Data.EventOrTimeSpan DecidedValue ->
  UISetsM PopulateError (Array (UI.EventOrTimeSpanPoly EventID TimeSpanID))
populateChildrenOrSiblings ids (Data.EventOrTimeSpan x) = case x of
  Left e@(Data.Event { id: id' }) -> do
    -- directly convert the leaf node, event
    addEvent (ConvertEvent.dataToUi e)
    let
      ids' = Array.snoc ids (UI.EventOrTimeSpanPoly (Left id'))
    pure ids'
  Right t@(Data.TimeSpan { id: id' }) -> do
    populateTimeSpan t
    let
      ids' = Array.snoc ids (UI.EventOrTimeSpanPoly (Right id'))
    pure ids'

-- | Recursively add a child `TimeSpan` to the sets
populateTimeSpan :: Data.TimeSpan DecidedValue -> UISetsM PopulateError Unit
populateTimeSpan ( Data.TimeSpan
    { timeSpace
  , name
  , description
  , id
  , span
  }
) = do
  -- add child time space and get its id
  mTimeSpace <- case timeSpace of
    Nothing -> pure Nothing
    Just timeSpaceDecided -> do
      populateTimeSpaceDecided timeSpaceDecided
      pure (Just (Data.getIdTimeSpaceDecided timeSpaceDecided))
  -- convert span to decided span
  span' <- case makeDecidedSpan span of
    Nothing -> throwError (TimeSpanMakeSpanFailed span)
    Just x -> pure x
  let
    timeSpan :: UI.TimeSpan
    timeSpan =
      UI.TimeSpan
        { name
        , description
        , span: span'
        , timeSpace: mTimeSpace
        , id
        }
  addTimeSpan timeSpan

synthesize :: UISets -> Effect (Either SynthesizeError Data.TimeSpaceDecided)
synthesize sets = do
  mRoot <- getRoot sets
  case mRoot of
    Nothing -> pure (Left NoRootExists)
    Just id -> runUISetsM (synthesizeTimeSpaceDecided id) sets

synthesizeTimeSpaceDecided :: TimeSpaceID -> UISetsM SynthesizeError Data.TimeSpaceDecided
synthesizeTimeSpaceDecided id = do
  Tuple decidedUnit timeSpace <- synthesizeTimeSpace id
  unsafePartial
    $ case decidedUnit of -- FIXME DecidedUnitFoo
        DecidedUnitNumber -> do
          let
            undecideValue :: DecidedValue -> UISetsM SynthesizeError Number
            undecideValue v = case v of
              DecidedValueNumber n -> pure n
              _ -> throwError (ConvertDecidedValueError { decidedUnit, decidedValue: v })
          Data.TimeSpaceNumber <$> traverse undecideValue timeSpace

synthesizeTimeSpace :: TimeSpaceID -> UISetsM SynthesizeError (Tuple DecidedUnit (Data.TimeSpace DecidedValue))
synthesizeTimeSpace id = do
  UI.TimeSpace
    { title
  , description
  , timeScale
  , siblings
  , timelines
  , id: id'
  } <-
    getTimeSpace id
  let
    { unit: decidedUnit, timeScale: timeScale' } = ConvertTimeScale.uiToData timeScale
  timelines' <- traverse synthesizeTimeline timelines
  siblings' <- traverse synthesizeEventOrTimeSpan siblings
  pure $ Tuple decidedUnit
    $ Data.TimeSpace
        { title
        , description
        , id: id'
        , timeScale: timeScale'
        , timelines: timelines'
        , siblings: siblings'
        }

synthesizeTimeline :: TimelineID -> UISetsM SynthesizeError (Data.Timeline DecidedValue)
synthesizeTimeline id = do
  UI.Timeline
    { name
  , description
  , children
  , id: id'
  } <-
    getTimeline id
  children' <- traverse synthesizeEventOrTimeSpan children
  pure
    $ Data.Timeline
        { name
        , description
        , id: id'
        , children: children'
        }

synthesizeEventOrTimeSpan :: UI.EventOrTimeSpanPoly EventID TimeSpanID -> UISetsM SynthesizeError (Data.EventOrTimeSpan DecidedValue)
synthesizeEventOrTimeSpan (UI.EventOrTimeSpanPoly eOrTs) = case eOrTs of
  Left eId ->
    Data.EventOrTimeSpan <<< Left <<< ConvertEvent.uiToData
      <$> getEvent eId
  Right tsId -> do
    UI.TimeSpan
      { name
    , description
    , span
    , timeSpace
    , id
    } <-
      getTimeSpan tsId
    timeSpace' <- case timeSpace of
      Nothing -> pure Nothing
      Just timeSpaceId -> Just <$> synthesizeTimeSpaceDecided timeSpaceId
    pure $ Data.EventOrTimeSpan $ Right
      $ Data.TimeSpan
          { name
          , description
          , span: unmakeDecidedSpan span
          , timeSpace: timeSpace'
          , id
          }

-- | Synthesize the rose tree for exploring time spaces.
synthesizeExploreTimeSpaces :: UISetsM SynthesizeError UI.ExploreTimeSpaces
synthesizeExploreTimeSpaces = asUISetsM' getExplorePredicate synthesizeExploreTimeSpacesScoped
  where
  getExplorePredicate uiSets =
    { timeSpacesMapping: S.readOnly (getTimeSpacesMapping uiSets)
    , timelinesMapping: S.readOnly (getTimelinesMapping uiSets)
    , timeSpansMapping: S.readOnly (getTimeSpansMapping uiSets)
    , rootRef: getRootRef uiSets
    }

synthesizeExploreTimeSpacesScoped ::
  { timeSpacesMapping :: IxSignalMap TimeSpaceID ( read :: S.READ ) UI.TimeSpace
  , timelinesMapping :: IxSignalMap TimelineID ( read :: S.READ ) UI.Timeline
  , timeSpansMapping :: IxSignalMap TimeSpanID ( read :: S.READ ) UI.TimeSpan
  , rootRef :: Ref (Maybe TimeSpaceID)
  } ->
  Effect (Either SynthesizeError UI.ExploreTimeSpaces)
synthesizeExploreTimeSpacesScoped { timeSpacesMapping, timelinesMapping, timeSpansMapping, rootRef } = do
  mRoot <- Ref.read rootRef
  case mRoot of
    Nothing -> pure (Left NoRootExists)
    Just id -> go id
  where
  go :: TimeSpaceID -> Effect (Either SynthesizeError UI.ExploreTimeSpaces)
  go timeSpaceId = do
    eTimeSpace <- getTimeSpaceScoped timeSpaceId timeSpacesMapping
    case eTimeSpace of
      Left e -> pure (Left e)
      Right
        ( UI.TimeSpace
          { title
        , timeScale: UI.TimeScale { limit }
        , id
        , timelines
        , siblings
        }
      ) -> do
        let
          getSpans :: Array (UI.EventOrTimeSpanPoly EventID TimeSpanID) -> Array TimeSpanID
          getSpans = Array.mapMaybe getSpan
            where
            getSpan (UI.EventOrTimeSpanPoly eOrTs) = case eOrTs of
              Left _ -> Nothing
              Right ts -> Just ts
        eSiblings <- traverse (flip getTimeSpanScoped timeSpansMapping) (getSpans siblings)
        case sequence eSiblings of
          Left e -> pure (Left e)
          Right siblings' -> do
            eTimelines <- traverse (flip getTimelineScoped timelinesMapping) timelines
            case sequence eTimelines of
              Left e -> pure (Left e)
              Right timelines' -> do
                let
                  childrenOfTimelines :: Array TimeSpanID
                  childrenOfTimelines =
                    Array.concatMap
                      (\(UI.Timeline { children }) -> getSpans children)
                      timelines'
                eChildren <- traverse (flip getTimeSpanScoped timeSpansMapping) childrenOfTimelines
                case sequence eChildren of
                  Left e -> pure (Left e)
                  Right children' -> do
                    let
                      siblingsWithTimeSpaces :: Array TimeSpaceID
                      siblingsWithTimeSpaces = Array.mapMaybe (\(UI.TimeSpan { timeSpace }) -> timeSpace) siblings'

                      childrenWithTimeSpaces :: Array TimeSpaceID
                      childrenWithTimeSpaces = Array.mapMaybe (\(UI.TimeSpan { timeSpace }) -> timeSpace) children'
                    -- recurse
                    eDescendants <- traverse go (siblingsWithTimeSpaces <> childrenWithTimeSpaces)
                    case sequence eDescendants of
                      Left e -> pure (Left e)
                      Right children ->
                        pure
                          $ Right
                          $ UI.ExploreTimeSpaces
                              { title
                              , limit
                              , id
                              , children
                              }
