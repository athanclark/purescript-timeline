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
  , tuple5
  , uncurry5
  , tuple6
  , uncurry6
  , get1
  , get2
  , get3
  , get4
  , get5
  )
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID, toBytes, parseBytesUUID) as UUID
import Data.UInt (fromInt, toInt) as UInt
import Data.NonEmpty (NonEmpty(..))
import Data.MultiSet.Indexed (IxMultiSet)
import Data.MultiSet.Indexed (mapKeys, fromFoldable) as IxMultiSet
import Data.IxSet (IxSet)
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
import Test.QuickCheck.Gen (arrayOf, oneOf, sized, resize)
import Test.QuickCheck.UTF8String (genString)
import Type.Proxy (Proxy(..))

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

instance functorTimeSpace :: Functor TimeSpace where
  map f (TimeSpace x) =
    TimeSpace
      x
        { timelines = map (map f) x.timelines
        , timeScale = map f x.timeScale
        }

derive newtype instance eqTimeSpace :: Ord index => Eq (TimeSpace index)

derive newtype instance ordTimeSpace :: Ord index => Ord (TimeSpace index)

derive newtype instance showTimeSpace :: Show index => Show (TimeSpace index)

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
      ~> jsonEmptyObject

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
  putArrayBuffer b o (TimeSpace { timeScale, timelines, title, description, document, id }) = putArrayBuffer b o (tuple6 timeScale timelines title description document fixedBytesId)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance decodeArrayBufferTimeSpace :: (DecodeArrayBuffer index, DynamicByteLength index, Ord index) => DecodeArrayBuffer (TimeSpace index) where
  readArrayBuffer b o = do
    let
      go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> Effect (Maybe (TimeSpace index))
      go =
        uncurry6 \timeScale timelines title description document id' -> case UUID.parseBytesUUID (getBytes id') of
          Nothing -> throw $ "Can't parse UUID: " <> show id'
          Just id -> pure $ Just $ TimeSpace { timeScale, timelines, title, description, document, id }
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> go xs
    where
    getBytes = map (\(Uint8 x) -> UInt.toInt x)

instance dynamicByteLengthTimeSpace :: DynamicByteLength index => DynamicByteLength (TimeSpace index) where
  byteLength (TimeSpace { timeScale, timelines, title, description, document, id }) = byteLength (tuple6 timeScale timelines title description document fixedBytesId)
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance arbitraryTimeSpace :: (Arbitrary index, Ord index) => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelines <- sized \s -> resize (s `div` 4) arbitrary
    title <- genString
    description <- genString
    document <- genString
    id <- pure (unsafePerformEffect UUID.genUUID)
    pure (TimeSpace { timeScale, timelines, title, description, document, id })

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
  , document :: String -- TODO markdown
  -- TODO color
  }

derive instance genericTimeline :: Generic (Timeline index) _

instance functorTimeline :: Functor Timeline where
  map f (Timeline x) = Timeline x { children = map (map f) x.children }

derive newtype instance eqTimeline :: Ord index => Eq (Timeline index)

derive newtype instance ordTimeline :: Ord index => Ord (Timeline index)

instance showTimeline :: Show index => Show (Timeline index) where
  show (Timeline { children, name, description, document }) =
    "(Timeline {children: " <> show children
      <> ", name: "
      <> show name
      <> ", description: "
      <> show description
      <> ", document: "
      <> show document
      <> "})"

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
        resize (size `div` 2) arbitrary
    -- let
    --   event = Tuple <$> (Instant <$> arbitrary) <*> (arrayOf (EventChild <$> arbitrary))
    --   timeSpan = Tuple <$> (Span <$> arbitrary) <*> (arrayOf (TimeSpanChild <$> arbitrary))
    -- (xs :: Array _) <- arrayOf $ oneOf $ NonEmpty event [ timeSpan ]
    -- pure (IxMultiSet.fromFoldable xs)
    name <- genString
    description <- genString
    document <- genString
    pure (Timeline { children, name, description, document })

-- ------------------ TimeSpan
-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan index
  = TimeSpan
  { timeSpace :: Maybe TimeSpaceDecided
  -- non-essential
  , name :: String
  , description :: String
  , document :: String -- TODO markdown
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
      <> ", document: "
      <> show x.document
      <> ", id: "
      <> show x.id
      <> ", span: "
      <> show x.span
      <> "}"

instance encodeJsonTimeSpan :: EncodeJson index => EncodeJson (TimeSpan index) where
  encodeJson (TimeSpan { timeSpace, name, description, document, id, span }) =
    "timeSpace" := timeSpace
      ~> "name"
      := name
      ~> "description"
      := description
      ~> "document"
      := document
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
    document <- o .: "document"
    id' <- o .: "id"
    span <- o .: "span" >>= decodeJsonSpan (Proxy :: Proxy index)
    case UUID.parseUUID id' of
      Nothing -> fail $ "Couldn't parse UUID: " <> id'
      Just id -> pure $ TimeSpan { timeSpace, name, description, document, id, span }

instance encodeArrayBufferTimeSpan :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpan index) where
  putArrayBuffer b o (TimeSpan { timeSpace, name, description, document, id, span }) = do
    mW <- putArrayBuffer b o (tuple5 timeSpace name description document fixedBytesId)
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBufferSpan (Proxy :: Proxy index) b (o + w) span
        case mW' of
          Nothing -> pure (Just w)
          Just w' -> pure (Just (w + w'))
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance decodeArrayBufferTimeSpan :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeSpan index) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just (xs :: _ /\ _ /\ _ /\ _ /\ _ /\ Unit) -> do
        l <- byteLength xs
        mSpan <- readArrayBufferSpan (Proxy :: Proxy index) b (o + l)
        case mSpan of
          Nothing -> pure Nothing
          Just span -> case UUID.parseBytesUUID (getBytes (get5 xs)) of
            Nothing -> throw $ "Couldn't parse UUID: " <> show (get5 xs)
            Just id ->
              pure $ Just
                $ TimeSpan
                    { timeSpace: get1 xs
                    , name: get2 xs
                    , description: get3 xs
                    , document: get4 xs
                    , id
                    , span
                    }
    where
    getBytes = map (\(Uint8 x) -> UInt.toInt x)

instance dynamicByteLengthTimeSpan :: DynamicByteLength index => DynamicByteLength (TimeSpan index) where
  byteLength (TimeSpan { timeSpace, name, description, document, id, span }) = do
    l <- byteLengthSpan (Proxy :: Proxy index) span
    l' <- byteLength (tuple5 timeSpace name description document fixedBytesId)
    pure (l + l')
    where
    fixedBytesId = map (Uint8 <<< UInt.fromInt) (UUID.toBytes id)

instance arbitraryTimeSpan :: Arbitrary index => Arbitrary (TimeSpan index) where
  arbitrary = do
    timeSpace <-
      let
        subtree = sized \s -> resize (s `div` 4) arbitrary
      in
        oneOf (NonEmpty (pure Nothing) [ Just <$> subtree ])
    name <- genString
    description <- genString
    document <- genString
    let
      id = unsafePerformEffect UUID.genUUID
    span <- genSpan (Proxy :: Proxy index)
    pure (TimeSpan { timeSpace, name, description, document, id, span })
