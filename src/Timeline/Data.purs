module Timeline.Data
  ( TimeSpan (..)
  , moveTimeSpanStart, moveTimeSpanStop
  , shiftLeftTimeSpan, shiftRightTimeSpan
  , Event (..)
  , moveEvent, shiftLeftEvent, shiftRightEvent
  , TimelineChild (..)
  , Timeline (..)
  , TimeScale (..)
  , changeTimeScaleBegin, changeTimeScaleEnd
  , TimeSpace (..)
  , TimeSpaceDecided (..)
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Enum (toEnumWithDefaults)
import Data.String.CodeUnits (fromCharArray)
import Data.NonEmpty (NonEmpty (..))
import Data.Tuple.Nested (type (/\), tuple4, uncurry4, tuple5, uncurry5, tuple6, uncurry6)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , putArrayBuffer, readArrayBuffer, byteLength)
import Data.ArrayBuffer.Class.Types (Int8 (..), Float64BE (..))
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, arrayOf, oneOf, sized, resize)



-- ------------------ TimeSpace

-- | A space where all time-definable values are stored; timelines, events, and time spans. Also defines how time
-- | is represented spatially; a time-scale.
newtype TimeSpace index = TimeSpace
  { timeScale :: TimeScale index
  , timelines :: Array (Timeline index)
  -- FIXME add cross-referenced timeline children? Ones that _reference_ multiple timelines, rather than belong to them
  -- non-essential
  , title       :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO markers, metrics & graduation
  }
derive instance genericTimeSpace :: Generic (TimeSpace index) _
derive newtype instance eqTimeSpace :: Ord index => Eq (TimeSpace index)
derive newtype instance ordTimeSpace :: Ord index => Ord (TimeSpace index)
instance functorTimeSpace :: Functor TimeSpace where
  map f (TimeSpace x) = TimeSpace x {timeScale = map f x.timeScale, timelines = map (map f) x.timelines}
derive newtype instance encodeJsonTimeSpace :: EncodeJson index => EncodeJson (TimeSpace index)
derive newtype instance decodeJsonTimeSpace :: DecodeJson index => DecodeJson (TimeSpace index)
instance encodeArrayBufferTimeSpace :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpace index) where
  putArrayBuffer b o (TimeSpace {timeScale,timelines,title,description,document}) =
    putArrayBuffer b o (tuple5 timeScale timelines title description document)
instance decodeArrayBufferTimeSpace :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeSpace index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeSpace index
        go = uncurry5 \timeScale timelines title description document ->
          TimeSpace {timeScale,timelines,title,description,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthTimeSpace :: DynamicByteLength index => DynamicByteLength (TimeSpace index) where
  byteLength (TimeSpace {timeScale,timelines,title,description,document}) =
    byteLength (tuple5 timeScale timelines title description document)
instance arbitraryTimeSpace :: Arbitrary index => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelines <- arbitrary
    title <- genString
    description <- genString
    document <- genString
    pure (TimeSpace {timeScale,timelines,title,description,document})



-- | All possible Human Time Indicies, with their instances assumed existing
data TimeSpaceDecided
  = TimeSpaceNumber (TimeSpace Number)
derive instance genericTimeSpaceDecided :: Generic TimeSpaceDecided _
instance eqTimeSpaceDecided :: Eq TimeSpaceDecided where
  eq = genericEq
instance ordTimeSpaceDecided :: Ord TimeSpaceDecided where
  compare = genericCompare
instance encodeJsonTimeSpaceDecided :: EncodeJson TimeSpaceDecided where
  encodeJson x = case x of
    TimeSpaceNumber y -> "number" := y ~> jsonEmptyObject
instance decodeJsonTimeSpaceDecided :: DecodeJson TimeSpaceDecided where
  decodeJson json = do
    o <- decodeJson json
    let decodeNumber = TimeSpaceNumber <$> o .: "number"
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
  byteLength x = map (_ + 1) $ case x of
    TimeSpaceNumber y -> byteLength (map Float64BE y)
instance arbitraryTimeSpaceDecided :: Arbitrary TimeSpaceDecided where
  arbitrary =
    oneOf $ NonEmpty
      (TimeSpaceNumber <$> arbitrary)
      []


-- TODO morphisms between decided types


-- ------------------ TimeScale

-- | Parameters for defining how time is represented spatially and numerically
newtype TimeScale index = TimeScale
  { beginIndex  :: index
  , endIndex    :: index
  -- TODO human <-> presented interpolation
  -- non-essential
  , name        :: String
  , description :: String
  , units       :: String
  , document    :: Maybe String -- TODO markdown
  }
derive instance genericTimeScale :: Generic (TimeScale index) _
derive newtype instance eqTimeScale :: Eq index => Eq (TimeScale index)
derive newtype instance ordTimeScale :: Ord index => Ord (TimeScale index)
instance functorTimeScale :: Functor TimeScale where
  map f (TimeScale x) = TimeScale x {beginIndex = f x.beginIndex, endIndex = f x.endIndex}
derive newtype instance encodeJsonTimeScale :: EncodeJson index => EncodeJson (TimeScale index)
derive newtype instance decodeJsonTimeScale :: DecodeJson index => DecodeJson (TimeScale index)
instance encodeArrayBufferTimeScale :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeScale index) where
  putArrayBuffer b o (TimeScale {beginIndex,endIndex,name,description,units,document}) =
    putArrayBuffer b o (tuple6 beginIndex endIndex name description units document)
instance decodeArrayBufferTimeScale :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeScale index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeScale index
        go = uncurry6 \beginIndex endIndex name description units document -> TimeScale {beginIndex,endIndex,name,description,units,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthTimeScale :: DynamicByteLength index => DynamicByteLength (TimeScale index) where
  byteLength (TimeScale {beginIndex,endIndex,name,description,units,document}) =
    byteLength (tuple6 beginIndex endIndex name description units document)
instance arbitraryTimeScale :: Arbitrary index => Arbitrary (TimeScale index) where
  arbitrary = do
    beginIndex <- arbitrary
    endIndex <- arbitrary
    name <- genString
    description <- genString
    units <- genString
    document <- oneOf (NonEmpty (pure Nothing) [Just <$> genString])
    pure (TimeScale {beginIndex,endIndex,name,description,units,document})

changeTimeScaleBegin :: forall index
                      . TimeScale index -> index
                     -> TimeScale index
changeTimeScaleBegin (TimeScale t) i = TimeScale t {beginIndex = i}

changeTimeScaleEnd :: forall index
                    . TimeScale index -> index
                   -> TimeScale index
changeTimeScaleEnd (TimeScale t) i = TimeScale t {endIndex = i}



-- ------------------ Timeline

-- | Types of children in a Timeline
data TimelineChild index
  = EventChild (Event index)
  | TimeSpanChild (TimeSpan index)
derive instance genericTimelineChild :: Generic (TimelineChild index) _
instance eqTimelineChild :: Eq index => Eq (TimelineChild index) where
  eq = genericEq
instance ordTimelineChild :: Ord index => Ord (TimelineChild index) where
  compare = genericCompare
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
    let decodeEvent = EventChild <$> o .: "event"
        decodeTimeSpan = TimeSpanChild <$> o .: "timeSpan"
    decodeEvent <|> decodeTimeSpan
instance encodeArrayBufferTimelineChild :: EncodeArrayBuffer index => EncodeArrayBuffer (TimelineChild index) where
  putArrayBuffer b o x = putArrayBuffer b o $ case x of
    EventChild y -> Left y
    TimeSpanChild y -> Right y
instance decodeArrayBufferTimelineChild :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimelineChild index) where
  readArrayBuffer b o =
    let fromEither eX = case eX of
          Left y -> EventChild y
          Right y -> TimeSpanChild y
    in  map fromEither <$> readArrayBuffer b o
instance dynamicByteLengthTimelineChild :: DynamicByteLength index => DynamicByteLength (TimelineChild index) where
  byteLength x = byteLength $ case x of
    EventChild y -> Left y
    TimeSpanChild y -> Right y
instance arbitraryTimelineChild :: Arbitrary index => Arbitrary (TimelineChild index) where
  arbitrary =
    oneOf (NonEmpty (EventChild <$> arbitrary) [TimeSpanChild <$> arbitrary])


-- | A set of Timeline children - events and timespans
newtype Timeline index = Timeline
  { children    :: Array (TimelineChild index)
  -- non-essential
  , name        :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO color
  }
derive instance genericTimeline :: Generic (Timeline index) _
derive newtype instance eqTimeline :: Ord index => Eq (Timeline index)
derive newtype instance ordTimeline :: Ord index => Ord (Timeline index)
instance functorTimeline :: Functor Timeline where
  map f (Timeline x) = Timeline x {children = map (map f) x.children}
derive newtype instance encodeJsonTimeline :: EncodeJson index => EncodeJson (Timeline index)
derive newtype instance decodeJsonTimeline :: DecodeJson index => DecodeJson (Timeline index)
instance encodeArrayBufferTimeline :: EncodeArrayBuffer index => EncodeArrayBuffer (Timeline index) where
  putArrayBuffer b o (Timeline {children,name,description,document}) =
    putArrayBuffer b o (tuple4 children name description document)
instance decodeArrayBufferTimeline :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (Timeline index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ Unit -> Timeline index
        go = uncurry4 \children name description document ->
          Timeline {children,name,description,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthTimeline :: DynamicByteLength index => DynamicByteLength (Timeline index) where
  byteLength (Timeline {children,name,description,document}) =
    byteLength (tuple4 children name description document)
instance arbitraryTimeline :: Arbitrary index => Arbitrary (Timeline index) where
  arbitrary = do
    children <- arbitrary
    name <- genString
    description <- genString
    document <- genString
    pure (Timeline {children,name,description,document})



-- ------------------ Event

-- | A punctuated event in time
newtype Event index = Event
  { timeIndex   :: index
  -- non-essential
  , name        :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO color
  }
derive instance genericEvent :: Generic (Event index) _
derive newtype instance eqEvent :: Eq index => Eq (Event index)
derive newtype instance ordEvent :: Ord index => Ord (Event index)
instance functorEvent :: Functor Event where
  map f (Event x) = Event x {timeIndex = f x.timeIndex}
derive newtype instance encodeJsonEvent :: EncodeJson index => EncodeJson (Event index)
derive newtype instance decodeJsonEvent :: DecodeJson index => DecodeJson (Event index)
instance encodeArrayBufferEvent :: EncodeArrayBuffer index => EncodeArrayBuffer (Event index) where
  putArrayBuffer b o (Event {timeIndex,name,description,document}) =
    putArrayBuffer b o (tuple4 timeIndex name description document)
instance decodeArrayBufferEvent :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (Event index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ Unit -> Event index
        go = uncurry4 \timeIndex name description document -> Event {timeIndex,name,description,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthEvent :: DynamicByteLength index => DynamicByteLength (Event index) where
  byteLength (Event {timeIndex,name,description,document}) =
    byteLength (tuple4 timeIndex name description document)
instance arbitraryEvent :: Arbitrary index => Arbitrary (Event index) where
  arbitrary = do
    timeIndex <- arbitrary
    name <- genString
    description <- genString
    document <- genString
    pure (Event {timeIndex,name,description,document})

-- | **Note**: Does not check for surrounding bounds
moveEvent :: forall index. Event index -> index -> Event index
moveEvent (Event x) i = Event x {timeIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftEvent :: forall index
                . Ring index
               => Event index -> index
               -> Event index
shiftLeftEvent (Event x) i = Event x {timeIndex = x.timeIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightEvent :: forall index
                 . Semiring index
                => Event index -> index
                -> Event index
shiftRightEvent (Event x) i = Event x {timeIndex = x.timeIndex + i}



-- ------------------ TimeSpan

-- | An event that lasts over some period of time, optionally containing its own time space (to be seen as a magnification of that period)
newtype TimeSpan index = TimeSpan
  { startIndex  :: index
  , stopIndex   :: index
  , timeSpace   :: Maybe TimeSpaceDecided
  -- non-essential
  , name        :: String
  , description :: String
  , document    :: String -- TODO markdown
  -- TODO color
  }
derive instance genericTimeSpan :: Generic (TimeSpan index) _
derive newtype instance eqTimeSpan :: Eq index => Eq (TimeSpan index)
derive newtype instance ordTimeSpan :: Ord index => Ord (TimeSpan index)
instance functorTimeSpan :: Functor TimeSpan where
  map f (TimeSpan x) = TimeSpan x {startIndex = f x.startIndex, stopIndex = f x.stopIndex}
derive newtype instance encodeJsonTimeSpan :: EncodeJson index => EncodeJson (TimeSpan index)
derive newtype instance decodeJsonTimeSpan :: DecodeJson index => DecodeJson (TimeSpan index)
instance encodeArrayBufferTimeSpan :: EncodeArrayBuffer index => EncodeArrayBuffer (TimeSpan index) where
  putArrayBuffer b o (TimeSpan {startIndex,stopIndex,timeSpace,name,description,document}) =
    putArrayBuffer b o (tuple6 startIndex stopIndex timeSpace name description document)
instance decodeArrayBufferTimeSpan :: (DecodeArrayBuffer index, DynamicByteLength index) => DecodeArrayBuffer (TimeSpan index) where
  readArrayBuffer b o =
    let go :: _ /\ _ /\ _ /\ _ /\ _ /\ _ /\ Unit -> TimeSpan index
        go = uncurry6 \startIndex stopIndex timeSpace name description document ->
          TimeSpan {startIndex,stopIndex,timeSpace,name,description,document}
    in  map go <$> readArrayBuffer b o
instance dynamicByteLengthTimeSpan :: DynamicByteLength index => DynamicByteLength (TimeSpan index) where
  byteLength (TimeSpan {startIndex,stopIndex,timeSpace,name,description,document}) =
    byteLength (tuple6 startIndex stopIndex timeSpace name description document)
instance arbitraryTimeSpan :: Arbitrary index => Arbitrary (TimeSpan index) where
  arbitrary = do
    startIndex <- arbitrary
    stopIndex <- arbitrary
    timeSpace <-
      let subtree = sized \s -> resize (s `div` 2) arbitrary
      in  oneOf (NonEmpty (pure Nothing) [Just <$> subtree])
    name <- genString
    description <- genString
    document <- genString
    pure (TimeSpan {startIndex,stopIndex,timeSpace,name,description,document})


-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStart :: forall index
                   . TimeSpan index -> index
                  -> TimeSpan index
moveTimeSpanStart (TimeSpan x) i = TimeSpan x {startIndex = i}

-- | **Note**: Does not check for surrounding bounds
moveTimeSpanStop :: forall index
                   . TimeSpan index -> index
                  -> TimeSpan index
moveTimeSpanStop (TimeSpan x) i = TimeSpan x {stopIndex = i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftLeftTimeSpan :: forall index
                   . Ring index
                  => TimeSpan index -> index
                  -> TimeSpan index
shiftLeftTimeSpan (TimeSpan x) i = TimeSpan x {startIndex = x.startIndex - i, stopIndex = x.stopIndex - i}

-- | **Note**: Does not check for surrounding bounds. Assumes value increases left-to-right
shiftRightTimeSpan :: forall index
                    . Semiring index
                   => TimeSpan index -> index
                   -> TimeSpan index
shiftRightTimeSpan (TimeSpan x) i = TimeSpan x {startIndex = x.startIndex + i, stopIndex = x.stopIndex + i}




-- --------------- Arbitrary Strings

genChar :: Gen Char
genChar = toEnumWithDefaults bottom top <$> withoutControlSeq
  where
    withoutControlSeq = chooseInt 0 (0xD800 - 1) <|> chooseInt (0xDFFF + 1) 65536

genString :: Gen String
genString = fromCharArray <$> arrayOf genChar
