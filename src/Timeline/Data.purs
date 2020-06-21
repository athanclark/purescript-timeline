module Timeline.Data
  ( TimeSpan(..)
  , TimelineChild(..)
  , Timeline(..)
  , TimeSpace(..)
  , TimeSpaceDecided(..)
  , module Timeline.Data.Event
  , module Timeline.Data.TimeScale
  ) where

import Timeline.Data.TimeComponent (InstantOrSpan(..))
import Timeline.Data.Event (Event(..))
import Timeline.Data.TimeScale (TimeScale(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), tuple4, uncurry4, tuple5, uncurry5, tuple6, uncurry6)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
import Data.NonEmpty (NonEmpty(..))
import Data.MultiSet.Indexed (IxMultiSet)
import Data.MultiSet.Indexed (mapKeys, fromFoldable) as IxMultiSet
import Data.IxSet (IxSet)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject, fail)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Int8(..), Float64BE(..))
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf, oneOf, sized, resize)
import Test.QuickCheck.UTF8String (genString)

-- ------------------ TimeSpace
-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
newtype TimeSpace index
  = TimeSpace
  { timeScale :: TimeScale index
  , timelines :: Array (Timeline index)
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  -- non-essential
  , title :: String
  , description :: String
  , document :: String -- TODO markdown
  , id :: UUID
  -- TODO markers, metrics & graduation
  }

derive instance genericTimeSpace :: Generic (TimeSpace index) _

derive newtype instance eqTimeSpace :: Ord index => Eq (TimeSpace index)

derive newtype instance ordTimeSpace :: Ord index => Ord (TimeSpace index)

derive newtype instance showTimeSpace :: (Show index, Ord index) => Show (TimeSpace index)

instance encodeJsonTimeSpace :: EncodeJson index => EncodeJson (TimeSpace index) where
  encodeJson (TimeSpace { timeScale, timelines, title, description, document, id }) =
    "timeScale" := timeScale
      ~> "timelines"
      := timelines
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "document"
      := document
      ~> "id"
      := UUID.toString id

instance decodeJsonTimeSpace :: (DecodeJson index, Ord index) => DecodeJson (TimeSpace index) where
  decodeJson json = do
    o <- decodeJson json
    timeScale <- o .: "timeScale"
    timelines <- o .: "timelines"
    title <- o .: "title"
    description <- o .: "description"
    document <- o .: "document"
    id' <- o .: "id"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Can't parse UUID: " <> id'
      Just id ->
        pure
          $ TimeSpace
              { timeScale, timelines, title, description, document, id }

instance encodeArrayBufferTimeSpace :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpace index) where
  putArrayBuffer b o (TimeSpace { timeScale, timelines, title, description, document, id }) = putArrayBuffer b o (tuple6 timeScale timelines title description document (UUID.toString id))

instance decodeArrayBufferTimeSpace :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index) => DecodeArrayBuffer (TimeSpace index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> Effect (TimeSpace index)
      go =
        uncurry6 \timeScale timelines title description document id' -> case UUID.parseUUID id' of
          Nothing -> throw $ "Can't parse UUID: " <> id'
          Just id -> pure $ TimeSpace { timeScale, timelines, title, description, document, id }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> Just <$> go xs

instance dynamicByteLengthTimeSpace :: DynamicByteLength index => DynamicByteLength (TimeSpace index) where
  byteLength (TimeSpace { timeScale, timelines, title, description, document, id }) = byteLength (tuple6 timeScale timelines title description document (UUID.toString id))

instance arbitraryTimeSpace :: (Arbitrary index, Ord index) => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelines <- arbitrary
    title <- genString
    description <- genString
    document <- genString
    id <- pure (unsafePerformEffect UUID.genUUID)
    pure (TimeSpace { timeScale, timelines, title, description, document, id })

mapTimeSpace :: forall index index'. Ord index' => (index -> index') -> TimeSpace index -> TimeSpace index'
mapTimeSpace f (TimeSpace x) =
  TimeSpace
    x
      { timelines = map (mapTimeline f) x.timelines
      , timeScale = map f x.timeScale
      }

-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided
  = TimeSpaceNumber (TimeSpace Number)

derive instance genericTimeSpaceDecided :: Generic TimeSpaceDecided _

instance eqTimeSpaceDecided :: Eq TimeSpaceDecided where
  eq x' y' = case Tuple x' y' of
    Tuple (TimeSpaceNumber x) (TimeSpaceNumber y) -> x == y
    _ -> false

instance ordTimeSpaceDecided :: Ord TimeSpaceDecided where
  compare = genericCompare

instance showTimeSpaceDecided :: Show TimeSpaceDecided where
  show = genericShow

instance encodeJsonTimeSpaceDecided :: EncodeJson TimeSpaceDecided where
  encodeJson x = case x of
    TimeSpaceNumber y -> "number" := y ~> jsonEmptyObject

instance decodeJsonTimeSpaceDecided :: DecodeJson TimeSpaceDecided where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeNumber = TimeSpaceNumber <$> o .: "number"
    decodeNumber

instance encodeArrayBufferTimeSpaceDecided :: EncodeArrayBuffer TimeSpaceDecided where
  putArrayBuffer b o x = case x of
    TimeSpaceNumber y -> do
      mW <- putArrayBuffer b o (Int8 0)
      case mW of
        Nothing -> pure Nothing
        Just w -> do
          mW' <- putArrayBuffer b (o + w) (mapTimeSpace Float64BE y)
          case mW' of
            Nothing -> pure (Just w)
            Just w' -> pure (Just (w + w'))

instance decodeArrayBufferTimeSpaceDecided :: DecodeArrayBuffer TimeSpaceDecided where
  readArrayBuffer b o = do
    mFlag <- readArrayBuffer b o
    case mFlag of
      Nothing -> pure Nothing
      Just (Int8 f)
        | f == 0 -> do
          mX <- readArrayBuffer b (o + 1)
          case mX of
            Nothing -> pure Nothing
            Just x -> pure $ Just $ TimeSpaceNumber $ mapTimeSpace (\(Float64BE y) -> y) x
        | otherwise -> pure Nothing

instance dynamicByteLengthTimeSpaceDecided :: DynamicByteLength TimeSpaceDecided where
  byteLength x =
    map (_ + 1)
      $ case x of
          TimeSpaceNumber y -> byteLength (mapTimeSpace Float64BE y)

instance arbitraryTimeSpaceDecided :: Arbitrary TimeSpaceDecided where
  arbitrary =
    oneOf
      $ NonEmpty
          (TimeSpaceNumber <$> arbitrary)
          []

-- TODO morphisms between decided types
-- ------------------ Timeline
-- | Types of children in a Timeline
data TimelineChild
  = EventChild Event
  | TimeSpanChild TimeSpan

derive instance genericTimelineChild :: Generic TimelineChild _

instance eqTimelineChild :: Eq TimelineChild where
  eq x' y' = case Tuple x' y' of
    Tuple (EventChild x) (EventChild y) -> x == y
    Tuple (TimeSpanChild x) (TimeSpanChild y) -> x == y
    _ -> false

instance ordTimelineChild :: Ord TimelineChild where
  compare = genericCompare

instance showTimelineChild :: Show TimelineChild where
  show = genericShow

instance encodeJsonTimelineChild :: EncodeJson TimelineChild where
  encodeJson x = case x of
    EventChild y -> "event" := y ~> jsonEmptyObject
    TimeSpanChild y -> "timeSpan" := y ~> jsonEmptyObject

instance decodeJsonTimelineChild :: DecodeJson TimelineChild where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeEvent = EventChild <$> o .: "event"

      decodeTimeSpan = TimeSpanChild <$> o .: "timeSpan"
    decodeEvent <|> decodeTimeSpan

instance encodeArrayBufferTimelineChild :: EncodeArrayBuffer TimelineChild where
  putArrayBuffer b o x =
    putArrayBuffer b o
      $ case x of
          EventChild y -> Left y
          TimeSpanChild y -> Right y

instance decodeArrayBufferTimelineChild :: DecodeArrayBuffer TimelineChild where
  readArrayBuffer b o =
    let
      fromEither eX = case eX of
        Left y -> EventChild y
        Right y -> TimeSpanChild y
    in
      map fromEither <$> readArrayBuffer b o

instance dynamicByteLengthTimelineChild :: DynamicByteLength TimelineChild where
  byteLength x =
    byteLength
      $ case x of
          EventChild y -> Left y
          TimeSpanChild y -> Right y

instance arbitraryTimelineChild :: Arbitrary TimelineChild where
  arbitrary = oneOf (NonEmpty (EventChild <$> arbitrary) [ TimeSpanChild <$> arbitrary ])

-- | A set of Timeline children - events and timespans
newtype Timeline index
  = Timeline
  { children :: IxMultiSet (InstantOrSpan index) TimelineChild
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  }

derive instance genericTimeline :: Generic (Timeline index) _

derive newtype instance eqTimeline :: Ord index => Eq (Timeline index)

derive newtype instance ordTimeline :: Ord index => Ord (Timeline index)

instance showTimeline :: Show index => Show (Timeline index) where
  show = genericShow

-- show (Timeline x) =
--   "Timeline {children: " <> show x.children
--   <> ", name: " <> show x.name
--   <> ", description: " <> show x.description
--   <> ", document: " <> show x.document
--   <> "}"
derive newtype instance encodeJsonTimeline :: EncodeJson index => EncodeJson (Timeline index)

derive newtype instance decodeJsonTimeline :: (DecodeJson index, Ord index) => DecodeJson (Timeline index)

instance encodeArrayBufferTimeline :: EncodeArrayBuffer index => EncodeArrayBuffer (Timeline index) where
  putArrayBuffer b o (Timeline { children, name, description, document }) = putArrayBuffer b o (tuple4 children name description document)

instance decodeArrayBufferTimeline :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index) => DecodeArrayBuffer (Timeline index) where
  readArrayBuffer b o =
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Timeline index
      go =
        uncurry4 \children name description document ->
          Timeline { children, name, description, document }
    in
      map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeline :: DynamicByteLength index => DynamicByteLength (Timeline index) where
  byteLength (Timeline { children, name, description, document }) = byteLength (tuple4 children name description document)

instance arbitraryTimeline :: (Arbitrary index, Ord index) => Arbitrary (Timeline index) where
  arbitrary = do
    children <-
      sized \size ->
        resize (size `div` 2) do
          let
            event = Tuple <$> (Instant <$> arbitrary) <*> (arrayOf (EventChild <$> arbitrary))

            timeSpan = Tuple <$> (Span <$> arbitrary) <*> (arrayOf (TimeSpanChild <$> arbitrary))
          (xs :: Array _) <- arrayOf $ oneOf $ NonEmpty event [ timeSpan ]
          pure (IxMultiSet.fromFoldable xs)
    name <- genString
    description <- genString
    document <- genString
    pure (Timeline { children, name, description, document })

mapTimeline :: forall index index'. Ord index' => (index -> index') -> Timeline index -> Timeline index'
mapTimeline f (Timeline x) = Timeline x { children = IxMultiSet.mapKeys (map f) x.children }

-- ------------------ TimeSpan
-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan
  = TimeSpan
  { timeSpace :: Maybe TimeSpaceDecided
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
  -- TODO color
  }

derive instance genericTimeSpan :: Generic TimeSpan _

derive newtype instance eqTimeSpan :: Eq TimeSpan

derive newtype instance ordTimeSpan :: Ord TimeSpan

instance showTimeSpan :: Show TimeSpan where
  show (TimeSpan x) =
    "TimeSpan {timeSpace: " <> show x.timeSpace
      <> ", name: "
      <> show x.name
      <> ", description: "
      <> show x.description
      <> ", document: "
      <> show x.document
      <> "}"

derive newtype instance encodeJsonTimeSpan :: EncodeJson TimeSpan

derive newtype instance decodeJsonTimeSpan :: DecodeJson TimeSpan

instance encodeArrayBufferTimeSpan :: EncodeArrayBuffer TimeSpan where
  putArrayBuffer b o (TimeSpan { timeSpace, name, description, document }) = putArrayBuffer b o (tuple4 timeSpace name description document)

instance decodeArrayBufferTimeSpan :: DecodeArrayBuffer TimeSpan where
  readArrayBuffer b o =
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> TimeSpan
      go =
        uncurry4 \timeSpace name description document ->
          TimeSpan { timeSpace, name, description, document }
    in
      map go <$> readArrayBuffer b o

instance dynamicByteLengthTimeSpan :: DynamicByteLength TimeSpan where
  byteLength (TimeSpan { timeSpace, name, description, document }) = byteLength (tuple4 timeSpace name description document)

instance arbitraryTimeSpan :: Arbitrary TimeSpan where
  arbitrary = do
    timeSpace <-
      let
        subtree = sized \s -> resize (s `div` 2) arbitrary
      in
        oneOf (NonEmpty (pure Nothing) [ Just <$> subtree ])
    name <- genString
    description <- genString
    document <- genString
    pure (TimeSpan { timeSpace, name, description, document })
