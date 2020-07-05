module Timeline.Convert where

import Timeline.Convert.UISets
  ( UISets(..)
  , addTimeSpace
  , addTimeline
  , addSiblingEvent
  , addSiblingTimeSpan
  , addChildEvent
  , addChildTimeSpan
  , setRoot
  , getTimeSpace
  , getTimeline
  , getSiblingEvent
  , getSiblingTimeSpan
  , getChildEvent
  , getChildTimeSpan
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
import Timeline.Time.Unit (DecidedUnit(..))
import Timeline.Time.Value (DecidedValue(..))
import Timeline.Time.Span (makeDecidedSpan, unmakeDecidedSpan)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), note)
import Data.Tuple (Tuple(..))
import Data.Array (snoc, foldM, mapMaybe, concatMap) as Array
import Data.Traversable (traverse)
import Data.UUID (UUID)
import Partial.Unsafe (unsafePartial) -- FIXME

-- | Translates a recursive `TimeSpaceDecided` into a flat `UISets`.
populate :: Data.TimeSpaceDecided -> Either PopulateError UISets
populate x =
  let
    xs = setRoot (Data.getIdTimeSpaceDecided x) mempty
  in
    populateTimeSpaceDecided x xs

-- | Recursively add a `TimeSpaceDecided` to the sets
populateTimeSpaceDecided :: Data.TimeSpaceDecided -> UISets -> Either PopulateError UISets
populateTimeSpaceDecided x sets = case x of
  Data.TimeSpaceNumber timeSpace -> populateTimeSpace DecidedUnitNumber (map DecidedValueNumber timeSpace) sets

-- | Recursively add a `TimeSpace` to the sets
populateTimeSpace :: DecidedUnit -> Data.TimeSpace DecidedValue -> UISets -> Either PopulateError UISets
populateTimeSpace decidedUnit ( Data.TimeSpace
    { timeScale
  , title
  , description
  , timelines
  , siblings
  , id
  }
) sets = do
  -- actually fold over the timelines
  Tuple timelineIds sets' <-
    Array.foldM populateTimelines (Tuple [] sets) timelines
  -- actually fold over the siblings
  Tuple siblingIds sets'' <-
    Array.foldM populateSiblings (Tuple [] sets') siblings
  -- convert the time scale
  timeScale' <-
    note
      (ConvertTimeScaleFailed { decidedUnit, timeScale })
      (ConvertTimeScale.dataToUi decidedUnit timeScale)
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
  addTimeSpace timeSpace' sets''
  where
  -- folds through the timelines, gathering ids as they're added to the sets
  populateTimelines ::
    Tuple (Array UUID) UISets ->
    Data.Timeline DecidedValue ->
    Either PopulateError (Tuple (Array UUID) UISets)
  populateTimelines (Tuple ids sets') timeline@(Data.Timeline { id: id' }) = do
    sets'' <- populateTimeline timeline sets'
    pure (Tuple (Array.snoc ids id') sets'')

  -- folds through the siblings, gathering ids as they're added to the sets
  populateSiblings ::
    Tuple (Array (UI.EventOrTimeSpanPoly UUID UUID)) UISets ->
    Data.EventOrTimeSpan DecidedValue ->
    Either PopulateError (Tuple (Array (UI.EventOrTimeSpanPoly UUID UUID)) UISets)
  populateSiblings (Tuple siblingIds sets'') (Data.EventOrTimeSpan x) = case x of
    Left e@(Data.Event { id: id' }) -> do
      -- directly convert the leaf node, event
      sets''' <- addSiblingEvent (ConvertEvent.dataToUi e) sets''
      let
        siblingIds' = Array.snoc siblingIds (UI.EventOrTimeSpanPoly (Left id'))
      pure (Tuple siblingIds' sets''')
    Right t@(Data.TimeSpan { id: id' }) -> do
      sets''' <- populateSiblingTimeSpan t sets''
      let
        siblingIds' = Array.snoc siblingIds (UI.EventOrTimeSpanPoly (Right id'))
      pure (Tuple siblingIds' sets''')

-- | Recursively add a `Timeline` to the sets
populateTimeline :: Data.Timeline DecidedValue -> UISets -> Either PopulateError UISets
populateTimeline (Data.Timeline { children, name, description, id }) sets = do
  -- actually fold over the siblings
  (Tuple childrenIds sets') <-
    Array.foldM populateChildren (Tuple [] sets) children
  let
    timeline' :: UI.Timeline
    timeline' =
      UI.Timeline
        { name
        , description
        , children: childrenIds
        , id
        }
  addTimeline timeline' sets'
  where
  -- folds through the children, gathering ids as they're added to the sets
  populateChildren ::
    Tuple (Array (UI.EventOrTimeSpanPoly UUID UUID)) UISets ->
    Data.EventOrTimeSpan DecidedValue ->
    Either PopulateError (Tuple (Array (UI.EventOrTimeSpanPoly UUID UUID)) UISets)
  populateChildren (Tuple childrenIds sets'') (Data.EventOrTimeSpan x) = case x of
    Left e@(Data.Event { id: id' }) -> do
      -- directly convert the leaf node, event
      sets''' <- addChildEvent (ConvertEvent.dataToUi e) sets''
      let
        childrenIds' = Array.snoc childrenIds (UI.EventOrTimeSpanPoly (Left id'))
      pure (Tuple childrenIds' sets''')
    Right t@(Data.TimeSpan { id: id' }) -> do
      sets''' <- populateChildTimeSpan t sets''
      let
        childrenIds' = Array.snoc childrenIds (UI.EventOrTimeSpanPoly (Right id'))
      pure (Tuple childrenIds' sets''')

-- | Recursively add a sibling `TimeSpan` to the sets
populateSiblingTimeSpan :: Data.TimeSpan DecidedValue -> UISets -> Either PopulateError UISets
populateSiblingTimeSpan ( Data.TimeSpan
    { timeSpace
  , name
  , description
  , id
  , span
  }
) sets = do
  -- add child time space and get its id
  Tuple mTimeSpace sets' <- case timeSpace of
    Nothing -> pure (Tuple Nothing sets)
    Just timeSpaceDecided -> do
      sets' <- populateTimeSpaceDecided timeSpaceDecided sets
      pure (Tuple (Just (Data.getIdTimeSpaceDecided timeSpaceDecided)) sets')
  -- convert span to decided span
  span' <- note (SiblingTimeSpanMakeSpanFailed span) (makeDecidedSpan span)
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
  addSiblingTimeSpan timeSpan sets'

-- | Recursively add a child `TimeSpan` to the sets
populateChildTimeSpan :: Data.TimeSpan DecidedValue -> UISets -> Either PopulateError UISets
populateChildTimeSpan ( Data.TimeSpan
    { timeSpace
  , name
  , description
  , id
  , span
  }
) sets = do
  -- add child time space and get its id
  Tuple mTimeSpace sets' <- case timeSpace of
    Nothing -> pure (Tuple Nothing sets)
    Just timeSpaceDecided -> do
      sets' <- populateTimeSpaceDecided timeSpaceDecided sets
      pure (Tuple (Just (Data.getIdTimeSpaceDecided timeSpaceDecided)) sets')
  -- convert span to decided span
  span' <- note (ChildTimeSpanMakeSpanFailed span) (makeDecidedSpan span)
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
  addChildTimeSpan timeSpan sets'

synthesize :: UISets -> Either SynthesizeError Data.TimeSpaceDecided
synthesize sets@(UISets xs) = case xs.root of
  Nothing -> Left NoRootExists
  Just id -> synthesizeTimeSpaceDecided id sets

synthesizeTimeSpaceDecided :: TimeSpaceID -> UISets -> Either SynthesizeError Data.TimeSpaceDecided
synthesizeTimeSpaceDecided id sets = do
  Tuple decidedUnit timeSpace <- synthesizeTimeSpace id sets
  unsafePartial
    $ case decidedUnit of -- FIXME DecidedUnitFoo
        DecidedUnitNumber -> do
          let
            undecideValue :: DecidedValue -> Either SynthesizeError Number
            undecideValue v = case v of
              DecidedValueNumber n -> pure n
              _ -> Left (ConvertDecidedValueError { decidedUnit, decidedValue: v })
          timeSpace' <- traverse undecideValue timeSpace
          pure (Data.TimeSpaceNumber timeSpace')

synthesizeTimeSpace :: TimeSpaceID -> UISets -> Either SynthesizeError (Tuple DecidedUnit (Data.TimeSpace DecidedValue))
synthesizeTimeSpace id sets = do
  UI.TimeSpace
    { title
  , description
  , timeScale
  , siblings
  , timelines
  , id: id'
  } <-
    getTimeSpace id sets
  let
    { unit: decidedUnit, timeScale: timeScale' } = ConvertTimeScale.uiToData timeScale
  timelines' <- traverse (flip synthesizeTimeline sets) timelines
  siblings' <- traverse (flip synthesizeSiblingEventOrTimeSpan sets) siblings
  pure $ Tuple decidedUnit
    $ Data.TimeSpace
        { title
        , description
        , id: id'
        , timeScale: timeScale'
        , timelines: timelines'
        , siblings: siblings'
        }

synthesizeTimeline :: UUID -> UISets -> Either SynthesizeError (Data.Timeline DecidedValue)
synthesizeTimeline id sets = do
  UI.Timeline
    { name
  , description
  , children
  , id: id'
  } <-
    getTimeline id sets
  children' <- traverse (flip synthesizeChildEventOrTimeSpan sets) children
  pure
    $ Data.Timeline
        { name
        , description
        , id: id'
        , children: children'
        }

synthesizeSiblingEventOrTimeSpan :: UI.EventOrTimeSpanPoly UUID UUID -> UISets -> Either SynthesizeError (Data.EventOrTimeSpan DecidedValue)
synthesizeSiblingEventOrTimeSpan (UI.EventOrTimeSpanPoly eOrTs) sets = case eOrTs of
  Left eId ->
    Data.EventOrTimeSpan
      <<< Left
      <<< ConvertEvent.uiToData
      <$> getSiblingEvent eId sets
  Right tsId -> do
    UI.TimeSpan
      { name
    , description
    , span
    , timeSpace
    , id
    } <-
      getSiblingTimeSpan tsId sets
    timeSpace' <- case timeSpace of
      Nothing -> pure Nothing
      Just timeSpaceId -> Just <$> synthesizeTimeSpaceDecided timeSpaceId sets
    pure $ Data.EventOrTimeSpan $ Right
      $ Data.TimeSpan
          { name
          , description
          , span: unmakeDecidedSpan span
          , timeSpace: timeSpace'
          , id
          }

synthesizeChildEventOrTimeSpan :: UI.EventOrTimeSpanPoly UUID UUID -> UISets -> Either SynthesizeError (Data.EventOrTimeSpan DecidedValue)
synthesizeChildEventOrTimeSpan (UI.EventOrTimeSpanPoly eOrTs) sets = case eOrTs of
  Left eId ->
    Data.EventOrTimeSpan
      <<< Left
      <<< ConvertEvent.uiToData
      <$> getChildEvent eId sets
  Right tsId -> do
    UI.TimeSpan
      { name
    , description
    , span
    , timeSpace
    , id
    } <-
      getChildTimeSpan tsId sets
    timeSpace' <- case timeSpace of
      Nothing -> pure Nothing
      Just timeSpaceId -> Just <$> synthesizeTimeSpaceDecided timeSpaceId sets
    pure $ Data.EventOrTimeSpan $ Right
      $ Data.TimeSpan
          { name
          , description
          , span: unmakeDecidedSpan span
          , timeSpace: timeSpace'
          , id
          }

-- | Synthesize the rose tree for exploring time spaces.
synthesizeExploreTimeSpaces :: UISets -> Either SynthesizeError UI.ExploreTimeSpaces
synthesizeExploreTimeSpaces sets@(UISets { root }) = case root of
  Nothing -> Left NoRootExists
  Just id -> go id
  where
  go :: TimeSpaceID -> Either SynthesizeError UI.ExploreTimeSpaces
  go timeSpaceId = do
    UI.TimeSpace
      { title
    , timeScale: UI.TimeScale { limit }
    , id
    , timelines
    , siblings
    } <-
      getTimeSpace timeSpaceId sets
    let
      getSpans :: Array (UI.EventOrTimeSpanPoly UUID UUID) -> Array UUID
      getSpans = Array.mapMaybe getSpan
        where
        getSpan (UI.EventOrTimeSpanPoly eOrTs) = case eOrTs of
          Left _ -> Nothing
          Right ts -> Just ts
    siblings' <- traverse (flip getSiblingTimeSpan sets) (getSpans siblings)
    children' <- do
      ts <- traverse (flip getTimeline sets) timelines
      let
        ts' =
          Array.concatMap
            (\(UI.Timeline { children }) -> getSpans children)
            ts
      traverse (flip getChildTimeSpan sets) ts'
    let
      siblings'' = Array.mapMaybe (\(UI.TimeSpan { timeSpace }) -> timeSpace) siblings'

      children'' = Array.mapMaybe (\(UI.TimeSpan { timeSpace }) -> timeSpace) children'
    -- recurse
    children <- traverse go (siblings'' <> children'')
    pure
      $ UI.ExploreTimeSpaces
          { title
          , limit
          , id
          , children
          }
