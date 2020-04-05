module Timeline.Data
  ( TimeSpan (..)
  , moveTimeSpanStart, moveTimeSpanStop
  , shiftLeftTimeSpan, shiftRightTimeSpan
  , Event (..)
  , moveEvent, shiftLeftEvent, shiftRightEvent
  , TimelineChild (..)
  , TimelineChildNum
  , Timeline (..)
  , insertTimelineChild, deleteTimelineChild, lookupTimelineChild
  , TimeScale (..)
  , changeTimeScaleBegin, changeTimeScaleEnd
  , TimelineNum
  , TimeSpace (..)
  , insertTimeline, deleteTimeline, lookupTimeline
  , TimeSpaceDecided (..)
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Enum (toEnumWithDefaults)
import Data.String.CodeUnits (fromCharArray)
import Data.NonEmpty (NonEmpty (..))
import Data.Tuple.Nested (type (/\), tuple4, uncurry4)
import Data.IxSet.Int (IxSet, Index, decodeJsonIxSet)
import Data.IxSet.Int (insert, delete, lookup, fromArray) as Ix
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , putArrayBuffer, readArrayBuffer, byteLength)
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
  , timelines :: IxSet (Timeline index) -- ^ Mutable reference
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
instance decodeJsonTimeSpace :: DecodeJson index => DecodeJson (TimeSpace index) where
  decodeJson json = do
    o <- decodeJson json
    timeScale <- o .: "timeScale"
    timelinesEffect <- o .: "timelines" >>= decodeJsonIxSet
    let timelines = unsafePerformEffect do
          {set} <- timelinesEffect
          pure set
    title <- o .: "title"
    description <- o .: "description"
    document <- o .: "document"
    pure (TimeSpace {timeScale,timelines,title,description,document})
instance arbitraryTimeSpace :: Arbitrary index => Arbitrary (TimeSpace index) where
  arbitrary = do
    timeScale <- arbitrary
    timelinesArray <- arbitrary
    let timelines = unsafePerformEffect do
          {set} <- Ix.fromArray timelinesArray
          pure set
    title <- genString
    description <- genString
    document <- genString
    pure (TimeSpace {timeScale,timelines,title,description,document})

-- | Alias to not confuse this with a TimeIndex
type TimelineNum = Index


insertTimeline :: forall index
                . TimeSpace index
               -> Timeline index
               -> Effect TimelineNum
insertTimeline (TimeSpace t) c = Ix.insert c t.timelines


deleteTimeline :: forall index
                . TimeSpace index
               -> TimelineNum
               -> Effect Unit
deleteTimeline (TimeSpace t) i = Ix.delete i t.timelines


lookupTimeline :: forall index
                . TimeSpace index
               -> TimelineNum
               -> Effect (Maybe (Timeline index))
lookupTimeline (TimeSpace t) i = Ix.lookup i t.timelines



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
instance arbitraryTimelineChild :: Arbitrary index => Arbitrary (TimelineChild index) where
  arbitrary =
    oneOf (NonEmpty (EventChild <$> arbitrary) [TimeSpanChild <$> arbitrary])

-- | Alias to not confuse this with a TimeIndex
type TimelineChildNum = Index

-- | A set of Timeline children - events and timespans
newtype Timeline index = Timeline
  { children    :: IxSet (TimelineChild index) -- ^ Mutable reference
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
instance decodeJsonTimeline :: DecodeJson index => DecodeJson (Timeline index) where
  decodeJson json = do
    o <- decodeJson json
    childrenEffect <- o .: "children" >>= decodeJsonIxSet
    let children = unsafePerformEffect do
          {set} <- childrenEffect
          pure set
    name <- o .: "name"
    description <- o .: "description"
    document <- o .: "document"
    pure (Timeline {children,name,description,document})
instance arbitraryTimeline :: Arbitrary index => Arbitrary (Timeline index) where
  arbitrary = do
    childrenArray <- arbitrary
    let children = unsafePerformEffect do
          {set} <- Ix.fromArray childrenArray
          pure set
    name <- genString
    description <- genString
    document <- genString
    pure (Timeline {children,name,description,document})


insertTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChild index
                    -> Effect TimelineChildNum
insertTimelineChild (Timeline t) c = Ix.insert c t.children


deleteTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChildNum
                    -> Effect Unit
deleteTimelineChild (Timeline t) i = Ix.delete i t.children


lookupTimelineChild :: forall index
                     . Timeline index
                    -> TimelineChildNum
                    -> Effect (Maybe (TimelineChild index))
lookupTimelineChild (Timeline t) i = Ix.lookup i t.children


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
-- instance encodeArrayBufferTimeSpan :: 
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
