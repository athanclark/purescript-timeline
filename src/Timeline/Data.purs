module Timeline.Data
  ( TimeSpan(..)
  , TimelineChild(..)
  , Timeline(..)
  , TimeSpace(..)
  , TimeSpaceDecided(..)
  , module Timeline.Data.Event
  , module Timeline.Data.TimeScale
  ) where

import Timeline.Time.Span
  ( Span
  , encodeJsonSpan
  , decodeJsonSpan
  , putArrayBufferSpan
  , readArrayBufferSpan
  , byteLengthSpan
  , genSpan
  )
import Timeline.Data.Event (Event(..))
import Timeline.Data.TimeScale (TimeScale(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested
  ( type (/\)
  , tuple4
  , uncurry4
  , tuple6
  , uncurry6
  , get1
  , get2
  , get3
  , get4
  )
import Data.Vec (Vec)
import Data.Typelevel.Num (D16)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID, toBytes, parseBytesUUID) as UUID
import Data.UInt (fromInt, toInt) as UInt
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject, fail)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Uint8(..), Int8(..), Float64BE(..))
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, sized, resize)
import Test.QuickCheck.UTF8String (genString)
import Type.Proxy (Proxy(..))

-- ------------------ TimeSpace
-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
newtype TimeSpace index
  = TimeSpace
  { timeScale :: TimeScale index
  , timelines :: Array (Timeline index)
  , siblings :: Array (TimelineChild index)
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  -- non-essential
  , title :: String
  , description :: String
  , id :: UUID
  -- TODO markers, metrics & graduation
  }

derive instance genericTimeSpace :: Generic (TimeSpace index) _

instance functorTimeSpace :: Functor TimeSpace where
  map f (TimeSpace x) =
    TimeSpace
      x
        { timelines = map (map f) x.timelines
        , siblings = map (map f) x.siblings
        , timeScale = map f x.timeScale
        }

derive newtype instance eqTimeSpace :: Ord index => Eq (TimeSpace index)

derive newtype instance ordTimeSpace :: Ord index => Ord (TimeSpace index)

derive newtype instance showTimeSpace :: Show index => Show (TimeSpace index)

instance encodeJsonTimeSpace :: EncodeJson index => EncodeJson (TimeSpace index) where
  encodeJson (TimeSpace { timeScale, timelines, siblings, title, description, id }) =
    "timeScale" := timeScale
      ~> "timelines"
      := timelines
      ~> "siblings"
      := siblings
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: (DecodeJson index, Ord index) => DecodeJson (TimeSpace index) where
  decodeJson json = do
    o <- decodeJson json
    timeScale <- o .: "timeScale"
    timelines <- o .: "timelines"
    siblings <- o .: "siblings"
    title <- o .: "title"
    description <- o .: "description"
    id' <- o .: "id"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Can't parse UUID: " <> id'
      Just id ->
        pure
          $ TimeSpace
              { timeScale, timelines, siblings, title, description, id }

instance encodeArrayBufferTimeSpace :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpace index) where
  putArrayBuffer b o (TimeSpace { timeScale, timelines, siblings, title, description, id }) = putArrayBuffer b o (tuple6 timeScale timelines siblings title description (toBytesUUID id))

instance decodeArrayBufferTimeSpace :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index) => DecodeArrayBuffer (TimeSpace index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe (TimeSpace index))
      go =
        uncurry6 \timeScale timelines siblings title description id' -> case parseBytesUUID id' of
          Nothing -> throw $ "Can't parse UUID: " <> show id'
          Just id -> pure $ Just $ TimeSpace { timeScale, timelines, siblings, title, description, id }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs

instance dynamicByteLengthTimeSpace :: DynamicByteLength index => DynamicByteLength (TimeSpace index) where
  byteLength (TimeSpace { timeScale, timelines, siblings, title, description, id }) = byteLength (tuple6 timeScale timelines siblings title description (toBytesUUID id))

instance arbitraryTimeSpace :: (Arbitrary index, Ord index) => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelines <- sized \s -> resize (s `div` 4) arbitrary
    siblings <- sized \s -> resize (s `div` 4) arbitrary
    title <- genString
    description <- genString
    id <- pure (unsafePerformEffect UUID.genUUID)
    pure (TimeSpace { timeScale, timelines, siblings, title, description, id })

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
  show x = case x of
    TimeSpaceNumber y -> "(TimeSpaceNumber " <> show y <> ")"

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
          mW' <- putArrayBuffer b (o + w) (map Float64BE y)
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
            Just x -> pure $ Just $ TimeSpaceNumber $ map (\(Float64BE y) -> y) x
        | otherwise -> pure Nothing

instance dynamicByteLengthTimeSpaceDecided :: DynamicByteLength TimeSpaceDecided where
  byteLength x =
    map (_ + 1)
      $ case x of
          TimeSpaceNumber y -> byteLength (map Float64BE y)

instance arbitraryTimeSpaceDecided :: Arbitrary TimeSpaceDecided where
  arbitrary =
    oneOf
      $ NonEmpty
          (TimeSpaceNumber <$> arbitrary)
          []

-- TODO morphisms between decided types
-- ------------------ Timeline
-- | Types of children in a Timeline
data TimelineChild index
  = EventChild (Event index)
  | TimeSpanChild (TimeSpan index)

derive instance genericTimelineChild :: Generic index index' => Generic (TimelineChild index) _

instance eqTimelineChild :: Eq index => Eq (TimelineChild index) where
  eq x' y' = case Tuple x' y' of
    Tuple (EventChild x) (EventChild y) -> x == y
    Tuple (TimeSpanChild x) (TimeSpanChild y) -> x == y
    _ -> false

-- FIXME special kind of comparison between types? Would it break pre-order?
instance ordTimelineChild :: Ord index => Ord (TimelineChild index) where
  compare x' y' = case Tuple x' y' of
    Tuple (EventChild _) (TimeSpanChild _) -> LT
    Tuple (TimeSpanChild _) (EventChild _) -> GT
    Tuple (EventChild x) (EventChild y) -> compare x y
    Tuple (TimeSpanChild x) (TimeSpanChild y) -> compare x y

instance showTimelineChild :: Show index => Show (TimelineChild index) where
  show x = case x of
    EventChild y -> "(EventChild " <> show y <> ")"
    TimeSpanChild y -> "(TimeSpanChild " <> show y <> ")"

instance functorTimelineChild :: Functor TimelineChild where
  map f x = case x of
    EventChild y -> EventChild (map f y)
    TimeSpanChild y -> TimeSpanChild (map f y)

instance encodeJsonTimelineChild :: EncodeJson index => EncodeJson (TimelineChild index) where
  encodeJson x = case x of
    EventChild y -> "event" := y ~> jsonEmptyObject
    TimeSpanChild y -> "timeSpan" := y ~> jsonEmptyObject

instance decodeJsonTimelineChild :: DecodeJson index => DecodeJson (TimelineChild index) where
  decodeJson json = do
    o <- decodeJson json
    let
      decodeEvent = EventChild <$> o .: "event"

      decodeTimeSpan = TimeSpanChild <$> o .: "timeSpan"
    decodeEvent <|> decodeTimeSpan

instance encodeArrayBufferTimelineChild :: EncodeArrayBuffer index => EncodeArrayBuffer (TimelineChild index) where
  putArrayBuffer b o x =
    putArrayBuffer b o
      $ case x of
          EventChild y -> Left y
          TimeSpanChild y -> Right y

instance decodeArrayBufferTimelineChild :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimelineChild index) where
  readArrayBuffer b o =
    let
      fromEither eX = case eX of
        Left y -> EventChild y
        Right y -> TimeSpanChild y
    in
      map fromEither <$> readArrayBuffer b o

instance dynamicByteLengthTimelineChild :: DynamicByteLength index => DynamicByteLength (TimelineChild index) where
  byteLength x =
    byteLength
      $ case x of
          EventChild y -> Left y
          TimeSpanChild y -> Right y

instance arbitraryTimelineChild :: Arbitrary index => Arbitrary (TimelineChild index) where
  arbitrary = oneOf (NonEmpty (EventChild <$> arbitrary) [ TimeSpanChild <$> arbitrary ])

-- | A set of Timeline children - events and timespans
newtype Timeline index
  = Timeline
  { children :: Array (TimelineChild index) -- TODO optional auxillary sorting data
  -- non-essential
  , name :: String
  , description :: String
  -- TODO color
  , id :: UUID
  }

derive instance genericTimeline :: Generic (Timeline index) _

instance functorTimeline :: Functor Timeline where
  map f (Timeline x) = Timeline x { children = map (map f) x.children }

derive newtype instance eqTimeline :: Ord index => Eq (Timeline index)

derive newtype instance ordTimeline :: Ord index => Ord (Timeline index)

instance showTimeline :: Show index => Show (Timeline index) where
  show (Timeline { children, name, description, id }) =
    "(Timeline {children: " <> show children
      <> ", name: "
      <> show name
      <> ", description: "
      <> show description
      <> ", id: "
      <> show id
      <> "})"

instance encodeJsonTimeline :: EncodeJson index => EncodeJson (Timeline index) where
  encodeJson (Timeline { children, name, description, id }) =
    "children" := children
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeline :: (DecodeJson index, Ord index) => DecodeJson (Timeline index) where
  decodeJson json = do
    o <- decodeJson json
    children <- o .: "children"
    name <- o .: "name"
    description <- o .: "description"
    id' <- o .: "id"
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure (Timeline { children, name, description, id })

instance encodeArrayBufferTimeline :: EncodeArrayBuffer index => EncodeArrayBuffer (Timeline index) where
  putArrayBuffer b o (Timeline { children, name, description, id }) = putArrayBuffer b o (tuple4 children name description (toBytesUUID id))

instance decodeArrayBufferTimeline :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index) => DecodeArrayBuffer (Timeline index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe (Timeline index))
      go =
        uncurry4 \children name description id' -> case parseBytesUUID id' of
          Nothing -> throw $ "Couldn't parse UUID: " <> show id'
          Just id -> pure $ Just $ Timeline { children, name, description, id }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs

instance dynamicByteLengthTimeline :: DynamicByteLength index => DynamicByteLength (Timeline index) where
  byteLength (Timeline { children, name, description, id }) = byteLength (tuple4 children name description (toBytesUUID id))

instance arbitraryTimeline :: (Arbitrary index, Ord index) => Arbitrary (Timeline index) where
  arbitrary = do
    children <-
      sized \size ->
        resize (size `div` 2) arbitrary
    name <- genString
    description <- genString
    let
      id = unsafePerformEffect UUID.genUUID
    pure (Timeline { children, name, description, id })

-- ------------------ TimeSpan
-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan index
  = TimeSpan
  { timeSpace :: Maybe TimeSpaceDecided
  -- non-essential
  , name :: String
  , description :: String
  -- TODO color
  , id :: UUID
  , span :: Span index
  }

derive instance genericTimeSpan :: Generic index index' => Generic (TimeSpan index) _

instance functorTimeSpan :: Functor TimeSpan where
  map f (TimeSpan x) = TimeSpan x { span = { start: f x.span.start, stop: f x.span.stop } }

derive newtype instance eqTimeSpan :: Eq index => Eq (TimeSpan index)

derive newtype instance ordTimeSpan :: Ord index => Ord (TimeSpan index) -- FIXME sort by span first?

instance showTimeSpan :: Show index => Show (TimeSpan index) where
  show (TimeSpan x) =
    "TimeSpan {timeSpace: " <> show x.timeSpace
      <> ", name: "
      <> show x.name
      <> ", description: "
      <> show x.description
      <> ", id: "
      <> show x.id
      <> ", span: "
      <> show x.span
      <> "}"

instance encodeJsonTimeSpan :: EncodeJson index => EncodeJson (TimeSpan index) where
  encodeJson (TimeSpan { timeSpace, name, description, id, span }) =
    "timeSpace" := timeSpace
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "id"
      := UUID.toString id
      ~> "span"
      := encodeJsonSpan (Proxy :: Proxy index) span
      ~> jsonEmptyObject

instance decodeJsonTimeSpan :: DecodeJson index => DecodeJson (TimeSpan index) where
  decodeJson json = do
    o <- decodeJson json
    timeSpace <- o .: "timeSpace"
    name <- o .: "name"
    description <- o .: "description"
    id' <- o .: "id"
    span <- o .: "span" >>= decodeJsonSpan (Proxy :: Proxy index)
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure $ TimeSpan { timeSpace, name, description, id, span }

instance encodeArrayBufferTimeSpan :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpan index) where
  putArrayBuffer b o (TimeSpan { timeSpace, name, description, id, span }) = do
    mW <- putArrayBuffer b o (tuple4 timeSpace name description (toBytesUUID id))
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBufferSpan (Proxy :: Proxy index) b (o + w) span
        case mW' of
          Nothing -> pure (Just w)
          Just w' -> pure (Just (w + w'))

instance decodeArrayBufferTimeSpan :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeSpan index) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just (xs :: _ /\ _ /\ _ /\ _ /\ Unit) -> do
        l <- byteLength xs
        mSpan <- readArrayBufferSpan (Proxy :: Proxy index) b (o + l)
        case mSpan of
          Nothing -> pure Nothing
          Just span -> case parseBytesUUID (get4 xs) of
            Nothing -> throw $ "Couldn't parse UUID: " <> show (get4 xs)
            Just id ->
              pure $ Just
                $ TimeSpan
                    { timeSpace: get1 xs
                    , name: get2 xs
                    , description: get3 xs
                    , id
                    , span
                    }

instance dynamicByteLengthTimeSpan :: DynamicByteLength index => DynamicByteLength (TimeSpan index) where
  byteLength (TimeSpan { timeSpace, name, description, id, span }) = do
    l <- byteLengthSpan (Proxy :: Proxy index) span
    l' <- byteLength (tuple4 timeSpace name description (toBytesUUID id))
    pure (l + l')

instance arbitraryTimeSpan :: Arbitrary index => Arbitrary (TimeSpan index) where
  arbitrary = do
    timeSpace <-
      let
        subtree = sized \s -> resize (s `div` 4) arbitrary
      in
        oneOf (NonEmpty (pure Nothing) [ Just <$> subtree ])
    name <- genString
    description <- genString
    let
      id = unsafePerformEffect UUID.genUUID
    span <- genSpan (Proxy :: Proxy index)
    pure (TimeSpan { timeSpace, name, description, id, span })

-- Utilities
parseBytesUUID :: Vec D16 Uint8 -> Maybe UUID
parseBytesUUID = UUID.parseBytesUUID <<< map (\(Uint8 x) -> UInt.toInt x)

toBytesUUID :: UUID -> Vec D16 Uint8
toBytesUUID = map (Uint8 <<< UInt.fromInt) <<< UUID.toBytes
