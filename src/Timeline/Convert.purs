module Timeline.Convert where

import Timeline.Convert.TimeScale (dataToUi, uiToData) as ConvertTimeScale
import Timeline.Convert.Event (dataToUi, uiToData) as ConvertEvent
import Timeline.UI.TimeSpace (TimeSpace(..)) as UI
import Timeline.UI.Timeline (Timeline(..)) as UI
import Timeline.UI.Event (Event(..)) as UI
import Timeline.UI.TimeSpan (TimeSpan(..)) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..)) as UI
import Timeline.Data
  ( TimeSpaceDecided(..)
  , TimeSpace(..)
  , TimeScale
  , Timeline(..)
  , EventOrTimeSpan(..)
  , TimeSpan(..)
  , Event(..)
  , getIdTimeSpaceDecided
  )
  as Data
import Timeline.Time.Unit (DecidedUnit(..))
import Timeline.Time.Value (DecidedValue(..))
import Timeline.Time.Span (Span, makeDecidedSpan, unmakeDecidedSpan)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), note)
import Data.Tuple (Tuple(..))
import Data.Array (snoc, foldM) as Array
import Data.Traversable (traverse)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.UUID (UUID)
import Data.UUID (toString) as UUID
import Foreign.Object (Object)
import Foreign.Object (union, empty, lookup, insert, member) as Object
import Partial.Unsafe (unsafePartial) -- FIXME

-- | Sets for all content, indexed by their UUID
newtype UISets
  = UISets
  { timeSpaces :: Object UI.TimeSpace
  , timelines :: Object UI.Timeline
  , siblingEvents :: Object UI.Event
  , siblingTimeSpans :: Object UI.TimeSpan
  , childEvents :: Object UI.Event
  , childTimeSpans :: Object UI.TimeSpan
  , root :: Maybe { id :: UUID }
  }

-- FIXME may need reference to their parent?
derive instance genericUISets :: Generic UISets _

derive newtype instance showUISets :: Show UISets

instance semigroupUISets :: Semigroup UISets where
  append (UISets x) (UISets y) =
    UISets
      { timeSpaces: Object.union y.timeSpaces x.timeSpaces
      , timelines: Object.union y.timelines x.timelines
      , siblingEvents: Object.union y.siblingEvents x.siblingEvents
      , siblingTimeSpans: Object.union y.siblingTimeSpans x.siblingTimeSpans
      , childEvents: Object.union y.childEvents x.childEvents
      , childTimeSpans: Object.union y.childTimeSpans x.childTimeSpans
      , root:
          case y.root of
            Nothing -> x.root
            _ -> y.root
      }

instance monoidUISets :: Monoid UISets where
  mempty =
    UISets
      { timeSpaces: Object.empty
      , timelines: Object.empty
      , siblingEvents: Object.empty
      , siblingTimeSpans: Object.empty
      , childEvents: Object.empty
      , childTimeSpans: Object.empty
      , root: Nothing
      }

data PopulateError
  = ConvertTimeScaleFailed
    { decidedUnit :: DecidedUnit
    , timeScale :: Data.TimeScale DecidedValue
    }
  | TimeSpaceExists UI.TimeSpace
  | TimelineExists UI.Timeline
  | SiblingEventExists UI.Event
  | SiblingTimeSpanExists UI.TimeSpan
  | SiblingTimeSpanMakeSpanFailed (Span DecidedValue)
  | ChildEventExists UI.Event
  | ChildTimeSpanExists UI.TimeSpan
  | ChildTimeSpanMakeSpanFailed (Span DecidedValue)

derive instance genericPopulateError :: Generic PopulateError _

instance showPopulateError :: Show PopulateError where
  show = genericShow

data SynthesizeError
  = TimeSpaceDoesntExist UUID
  | TimelineDoesntExist UUID
  | SiblingEventDoesntExist UUID
  | SiblingTimeSpanDoesntExist UUID
  | ChildEventDoesntExist UUID
  | ChildTimeSpanDoesntExists UUID
  | NoRootExists UISets
  | ConvertDecidedValueError
    { decidedUnit :: DecidedUnit
    , decidedValue :: DecidedValue
    }

derive instance genericSynthesizeError :: Generic SynthesizeError _

instance showSynthesizeError :: Show SynthesizeError where
  show = genericShow

-- | Includes an already flat time space - doesn't verify constituents
addTimeSpace :: UI.TimeSpace -> UISets -> Either PopulateError UISets
addTimeSpace x@(UI.TimeSpace { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.timeSpaces then
      Left (TimeSpaceExists x)
    else
      Right
        $ UISets
            xs
              { timeSpaces = Object.insert id' x xs.timeSpaces
              }

-- | Looks for an already flat time space in the sets
getTimeSpace :: UUID -> UISets -> Either SynthesizeError UI.TimeSpace
getTimeSpace id (UISets { timeSpaces }) = note (TimeSpaceDoesntExist id) (Object.lookup (UUID.toString id) timeSpaces)

-- | Includes an already flat timeline - doesn't verify constituents
addTimeline :: UI.Timeline -> UISets -> Either PopulateError UISets
addTimeline x@(UI.Timeline { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.timelines then
      Left (TimelineExists x)
    else
      Right
        $ UISets
            xs
              { timelines = Object.insert id' x xs.timelines
              }

-- | Looks for an already flat timeline in the sets
getTimeline :: UUID -> UISets -> Either SynthesizeError UI.Timeline
getTimeline id (UISets { timelines }) = note (TimelineDoesntExist id) (Object.lookup (UUID.toString id) timelines)

-- | Includes an already flat event as a sibling - doesn't verify constituents
addSiblingEvent :: UI.Event -> UISets -> Either PopulateError UISets
addSiblingEvent x@(UI.Event { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.siblingEvents then
      Left (SiblingEventExists x)
    else
      Right
        $ UISets
            xs
              { siblingEvents = Object.insert id' x xs.siblingEvents
              }

-- | Looks for an already flat event (as a sibling) in the sets
getSiblingEvent :: UUID -> UISets -> Either SynthesizeError UI.Event
getSiblingEvent id (UISets { siblingEvents }) = note (SiblingEventDoesntExist id) (Object.lookup (UUID.toString id) siblingEvents)

-- | Includes an already flat time span as a sibling - doesn't verify constituents
addSiblingTimeSpan :: UI.TimeSpan -> UISets -> Either PopulateError UISets
addSiblingTimeSpan x@(UI.TimeSpan { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.siblingTimeSpans then
      Left (SiblingTimeSpanExists x)
    else
      Right
        $ UISets
            xs
              { siblingTimeSpans = Object.insert id' x xs.siblingTimeSpans
              }

-- | Looks for an already flat time span (as a sibling) in the sets
getSiblingTimeSpan :: UUID -> UISets -> Either SynthesizeError UI.TimeSpan
getSiblingTimeSpan id (UISets { siblingTimeSpans }) = note (SiblingTimeSpanDoesntExist id) (Object.lookup (UUID.toString id) siblingTimeSpans)

-- | Includes an already flat event as a child - doesn't verify constituents
addChildEvent :: UI.Event -> UISets -> Either PopulateError UISets
addChildEvent x@(UI.Event { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.childEvents then
      Left (ChildEventExists x)
    else
      Right
        $ UISets
            xs
              { childEvents = Object.insert id' x xs.childEvents
              }

-- | Looks for an already flat event (as a child) in the sets
getChildEvent :: UUID -> UISets -> Either SynthesizeError UI.Event
getChildEvent id (UISets { childEvents }) = note (ChildEventDoesntExist id) (Object.lookup (UUID.toString id) childEvents)

-- | Includes an already flat time span as a child - doesn't verify constituents
addChildTimeSpan :: UI.TimeSpan -> UISets -> Either PopulateError UISets
addChildTimeSpan x@(UI.TimeSpan { id }) (UISets xs) =
  let
    id' = UUID.toString id
  in
    if Object.member id' xs.childTimeSpans then
      Left (ChildTimeSpanExists x)
    else
      Right
        $ UISets
            xs
              { childTimeSpans = Object.insert id' x xs.childTimeSpans
              }

-- | Looks for an already flat time span (as a child) in the sets
getChildTimeSpan :: UUID -> UISets -> Either SynthesizeError UI.TimeSpan
getChildTimeSpan id (UISets { childTimeSpans }) = note (ChildTimeSpanDoesntExists id) (Object.lookup (UUID.toString id) childTimeSpans)

-- | Assigns the root field of a set
setRoot :: UUID -> UISets -> UISets
setRoot id (UISets x) = UISets x { root = Just { id } }

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
  Nothing -> Left (NoRootExists sets)
  Just { id } -> synthesizeTimeSpaceDecided id sets

synthesizeTimeSpaceDecided :: UUID -> UISets -> Either SynthesizeError Data.TimeSpaceDecided
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

synthesizeTimeSpace :: UUID -> UISets -> Either SynthesizeError (Tuple DecidedUnit (Data.TimeSpace DecidedValue))
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
