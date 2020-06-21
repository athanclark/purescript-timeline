module Timeline.Data.TimeComponent where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  (class EncodeJson, class DecodeJson, (.:), (:=), (~>), jsonEmptyObject, decodeJson)
import Data.ArrayBuffer.Class
  ( class DynamicByteLength
  , class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, suchThat)

-- | Represents a period between to specific instances
newtype SpanOfTime a
  = SpanOfTime
  { startTime :: a
  , stopTime :: a
  }

derive instance genericSpanOfTime :: Generic a a' => Generic (SpanOfTime a) _

instance functorSpanOfTime :: Functor SpanOfTime where
  map f (SpanOfTime { startTime, stopTime }) = SpanOfTime { startTime: f startTime, stopTime: f stopTime }

derive newtype instance eqSpanOfTime :: Eq a => Eq (SpanOfTime a)

derive newtype instance ordSpanOfTime :: Ord a => Ord (SpanOfTime a)

derive newtype instance showSpanOfTime :: Show a => Show (SpanOfTime a)

instance encodeJsonSpanOfTime :: EncodeJson a => EncodeJson (SpanOfTime a) where
  encodeJson (SpanOfTime { startTime, stopTime }) = "startTime" := startTime ~> "stopTime" := stopTime ~> jsonEmptyObject

instance decodeJsonSpanOfTime :: DecodeJson a => DecodeJson (SpanOfTime a) where
  decodeJson json = do
    o <- decodeJson json
    startTime <- o .: "startTime"
    stopTime <- o .: "stopTime"
    pure (SpanOfTime { startTime, stopTime })

instance dynamicByteLengthSpanOfTime :: DynamicByteLength a => DynamicByteLength (SpanOfTime a) where
  byteLength (SpanOfTime { startTime, stopTime }) = do
    x <- byteLength startTime
    y <- byteLength stopTime
    pure (x + y)

instance encodeArrayBufferSpanOfTime :: EncodeArrayBuffer a => EncodeArrayBuffer (SpanOfTime a) where
  putArrayBuffer b o (SpanOfTime { startTime, stopTime }) = do
    mW <- putArrayBuffer b o startTime
    case mW of
      Nothing -> pure Nothing
      Just w -> do
        mW' <- putArrayBuffer b (o + w) stopTime
        case mW' of
          Nothing -> pure (Just w)
          Just w' -> pure (Just (w + w'))

instance decodeArrayBufferSpanOfTime :: (DynamicByteLength a, DecodeArrayBuffer a) => DecodeArrayBuffer (SpanOfTime a) where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just startTime -> do
        l <- byteLength startTime
        mY <- readArrayBuffer b (o + l)
        case mY of
          Nothing -> pure Nothing
          Just stopTime -> pure (Just (SpanOfTime { startTime, stopTime }))

instance arbitrarySpanOfTime :: (Arbitrary a, Ord a) => Arbitrary (SpanOfTime a) where
  arbitrary = do
    Tuple startTime stopTime <- arbitrary `suchThat` (\(Tuple startTime stopTime) -> startTime < stopTime)
    pure (SpanOfTime { startTime, stopTime })

-- | **Note**: Does not check for surrounding `TimeSpace` bounds.
setTimeSpanStart :: forall a. SpanOfTime a -> a -> SpanOfTime a
setTimeSpanStart (SpanOfTime x) i = SpanOfTime x { startTime = i }

-- | **Note**: Does not check for surrounding `TimeSpace` bounds.
setTimeSpanStop :: forall a. SpanOfTime a -> a -> SpanOfTime a
setTimeSpanStop (SpanOfTime x) i = SpanOfTime x { stopTime = i }

-- | **Note**: Does not check for surrounding `TimeSpace` bounds. Assumes value increases left-to-right
shiftLeftSpanOfTime ::
  forall index.
  Ring index =>
  SpanOfTime index ->
  index ->
  SpanOfTime index
shiftLeftSpanOfTime (SpanOfTime x) i = SpanOfTime { startTime: x.startTime - i, stopTime: x.stopTime - i }

-- | **Note**: Does not check for surrounding `TimeSpace` bounds. Assumes value increases left-to-right
shiftRightSpanOfTime ::
  forall index.
  Semiring index =>
  SpanOfTime index ->
  index ->
  SpanOfTime index
shiftRightSpanOfTime (SpanOfTime x) i = SpanOfTime { startTime: x.startTime + i, stopTime: x.stopTime + i }

-- | Represents _either_ an instant in time, or a span across two times.
-- | The comparison of "cross types" - i.e. between `Instant` and `Span`, should never be equal,
-- | so they don't overwrite each other when stored in a map.
data InstantOrSpan a
  = Instant a
  | Span (SpanOfTime a)

derive instance genericInstantOrSpan :: Generic a a' => Generic (InstantOrSpan a) _

instance functorInstantOrSpan :: Functor InstantOrSpan where
  map f x = case x of
    Instant y -> Instant (f y)
    Span y -> Span (map f y)

instance eqInstantOrSpan :: Eq a => Eq (InstantOrSpan a) where
  eq x' y' = case Tuple x' y' of
    Tuple (Instant x) (Instant y) -> x == y
    Tuple (Span x) (Span y) -> x == y
    _ -> false

instance ordInstantOrSpan :: Ord a => Ord (InstantOrSpan a) where
  compare (Instant x) (Instant y) = compare x y
  compare (Span x) (Span y) = compare x y
  compare (Instant _) (Span _) = LT
  compare (Span _) (Instant _) = GT

-- compare (Instant x) (Span (SpanOfTime {startTime})) = if x <= startTime then LT else GT
-- compare (Span (SpanOfTime {startTime})) (Instant y) = if y <= startTime then LT else GT
instance showInstantOrSpan :: Show a => Show (InstantOrSpan a) where
  show x = case x of
    Instant y -> "Instant (" <> show y <> ")"
    Span y -> "Span (" <> show y <> ")"

instance encodeJsonInstantOrSpan :: EncodeJson a => EncodeJson (InstantOrSpan a) where
  encodeJson x = case x of
    Instant y -> "instant" := y ~> jsonEmptyObject
    Span y -> "span" := y ~> jsonEmptyObject

instance decodeJsonInstantOrSpan :: DecodeJson a => DecodeJson (InstantOrSpan a) where
  decodeJson json = do
    o <- decodeJson json
    let
      instant = Instant <$> o .: "instant"

      span = Span <$> o .: "span"
    instant <|> span

instance dynamicByteLengthInstantOrSpan :: DynamicByteLength a => DynamicByteLength (InstantOrSpan a) where
  byteLength x =
    byteLength
      $ case x of
          Instant y -> Left y
          Span y -> Right y

instance encodeArrayBufferInstantOrSpan :: EncodeArrayBuffer a => EncodeArrayBuffer (InstantOrSpan a) where
  putArrayBuffer b o x =
    putArrayBuffer b o
      $ case x of
          Instant y -> Left y
          Span y -> Right y

instance decodeArrayBufferInstantOrSpan :: (DynamicByteLength a, DecodeArrayBuffer a) => DecodeArrayBuffer (InstantOrSpan a) where
  readArrayBuffer b o =
    let
      fromEither eX = case eX of
        Left y -> Instant y
        Right y -> Span y
    in
      map fromEither <$> readArrayBuffer b o

instance arbitraryInstantOrSpan :: (Arbitrary a, Ord a) => Arbitrary (InstantOrSpan a) where
  arbitrary = oneOf $ NonEmpty (Instant <$> arbitrary) [ Span <$> arbitrary ]
