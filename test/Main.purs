module Test.Main where

import Timeline.Data (TimeSpan, EventOrTimeSpan, Timeline, TimeSpace, TimeSpaceDecided) as Data
import Timeline.Data.Event (Event) as Data
import Timeline.Data.TimeScale (TimeScale) as Data
import Timeline.UI.Event (Event) as UI
import Timeline.UI.TimeSpan (TimeSpan) as UI
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan) as UI
import Timeline.UI.Settings (Settings) as UI
import Timeline.UI.Timeline (Timeline) as UI
import Timeline.UI.Timeline.Children (Children) as UI
import Timeline.UI.TimeSpace (TimeSpace) as UI
import Timeline.UI.TimeSpace.TimeScale (TimeScale) as UI
import Timeline.UI.TimeSpace.Timelines (Timelines) as UI
import Timeline.UI.TimeSpace.Siblings (Siblings) as UI
import Timeline.Convert (populate, synthesize) as Convert
import Timeline.Convert.UISets (new) as UISets

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Float64BE (..))
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, quickCheck', Result (..))
import Test.Spec (describe, it, SpecT)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] tests

tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Timeline.Data" do
    describe "Json" do
      jsonTest "TimeSpan" (Proxy :: Proxy (Data.TimeSpan Number))
      jsonTest "Event" (Proxy :: Proxy (Data.Event Number))
      jsonTest "EventOrTimeSpan" (Proxy :: Proxy (Data.EventOrTimeSpan Number))
      jsonTest "Timeline" (Proxy :: Proxy (Data.Timeline Number))
      jsonTest "TimeScale" (Proxy :: Proxy (Data.TimeScale Number))
      jsonTest "TimeSpace" (Proxy :: Proxy (Data.TimeSpace Number))
      jsonTest "TimeSpaceDecided" (Proxy :: Proxy Data.TimeSpaceDecided)
    describe "Binary" do
      binaryTest "TimeSpan" (Proxy :: Proxy (Data.TimeSpan BinaryFloat))
      binaryTest "Event" (Proxy :: Proxy (Data.Event BinaryFloat))
      binaryTest "EventOrTimeSpan" (Proxy :: Proxy (Data.EventOrTimeSpan BinaryFloat))
      binaryTest "Timeline" (Proxy :: Proxy (Data.Timeline BinaryFloat))
      binaryTest "TimeScale" (Proxy :: Proxy (Data.TimeScale BinaryFloat))
      binaryTest "TimeSpace" (Proxy :: Proxy (Data.TimeSpace BinaryFloat))
      binaryTest "TimeSpaceDecided" (Proxy :: Proxy Data.TimeSpaceDecided)
  describe "Timeline.UI" $
    describe "Json" do
      jsonTest "Event" (Proxy :: Proxy UI.Event)
      jsonTest "TimeSpan" (Proxy :: Proxy UI.TimeSpan)
      jsonTest "EventOrTimeSpan" (Proxy :: Proxy UI.EventOrTimeSpan)
      jsonTest "Settings" (Proxy :: Proxy UI.Settings)
      jsonTest "TimeScale" (Proxy :: Proxy UI.TimeScale)
      jsonTest "Timeline" (Proxy :: Proxy UI.Timeline)
      jsonTest "Timelines" (Proxy :: Proxy UI.Timelines)
      jsonTest "TimeSpace" (Proxy :: Proxy UI.TimeSpace)
      jsonTest "Children" (Proxy :: Proxy UI.Children)
      jsonTest "Siblings" (Proxy :: Proxy UI.Siblings)
  describe "Timeline.Convert" do
    let synthesizePopulateIso :: Data.TimeSpaceDecided -> Result
        synthesizePopulateIso x = case unsafePerformEffect (Convert.populate x) of
          Left popError -> Failed $ "Populate error: " <> show popError
          Right sets -> case unsafePerformEffect (Convert.synthesize sets) of
            Left synthError -> Failed $ "Synthesize error: " <> show synthError
            Right y
              | x == y -> Success
              | otherwise -> Failed $ "Not equal - x: " <> show x <> ", y: " <> show y
    it "synthesize <<< populate isomorphism" $
      liftEffect $ quickCheck' 1000 synthesizePopulateIso
  where
    jsonTest :: forall a
              . Arbitrary a
             => Show a
             => Eq a
             => EncodeJson a
             => DecodeJson a
             => String -> Proxy a -> _
    jsonTest name proxy = it name (liftEffect (quickCheck (jsonIso proxy)))
    binaryTest :: forall a
                . Arbitrary a
               => Show a
               => Eq a
               => EncodeArrayBuffer a
               => DecodeArrayBuffer a
               => DynamicByteLength a
               => String -> Proxy a -> _
    binaryTest name proxy = it name (liftEffect (quickCheck (binaryIso proxy)))


jsonIso :: forall a
         . Eq a
        => Show a
        => EncodeJson a
        => DecodeJson a
        => Proxy a -> a -> Result
jsonIso Proxy x =
  -- trace x \_ ->
  let result = decodeJson (encodeJson x)
  in  case result of
        Left e -> Failed $ "Couldn't parse: " <> e
        Right y
          | y == x -> Success
          | otherwise ->
              Failed $ "Not equal - original " <> show (show x) <> "\nresult: " <> show (show y)


binaryIso :: forall a
           . Eq a
          => Show a
          => EncodeArrayBuffer a
          => DecodeArrayBuffer a
          => DynamicByteLength a
          => Proxy a -> a -> Result
binaryIso Proxy x = unsafePerformEffect do
  buf <- encodeArrayBuffer x
  mY <- decodeArrayBuffer buf
  pure $
    if mY == Just x
      then Success
      else Failed $ "Not equal - original " <> show (show x) <> "\nresult: " <> show (show mY)


newtype BinaryFloat = BinaryFloat Float64BE
derive instance genericBinaryFloat :: Generic BinaryFloat _
derive newtype instance eqBinaryFloat :: Eq BinaryFloat
derive newtype instance ordBinaryFloat :: Ord BinaryFloat
derive newtype instance showBinaryFloat :: Show BinaryFloat
derive newtype instance encodeArrayBufferBinaryFloat :: EncodeArrayBuffer BinaryFloat
derive newtype instance decodeArrayBufferBinaryFloat :: DecodeArrayBuffer BinaryFloat
derive newtype instance dynamicByteLengthBinaryFloat :: DynamicByteLength BinaryFloat
instance arbitraryBinaryFloat :: Arbitrary BinaryFloat where
  arbitrary = BinaryFloat <<< Float64BE <$> arbitrary
