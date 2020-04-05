module Test.Main where

import Timeline.Data (TimeSpan, Event, TimelineChild, Timeline, TimeScale, TimeSpace, TimeSpaceDecided)

import Prelude
import Data.Maybe (Maybe (Nothing))
import Data.Either (Either (Right))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, quickCheck)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Type.Proxy (Proxy (..))

main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] tests

tests :: SpecT Aff Unit Identity Unit
tests = do
  describe "Json" do
    jsonTest "TimeSpan" (Proxy :: Proxy (TimeSpan Number))
    jsonTest "Event" (Proxy :: Proxy (Event Number))
    jsonTest "TimelineChild" (Proxy :: Proxy (TimelineChild Number))
    jsonTest "Timeline" (Proxy :: Proxy (Timeline Number))
    jsonTest "TimeScale" (Proxy :: Proxy (TimeScale Number))
    jsonTest "TimeSpace" (Proxy :: Proxy (TimeSpace Number))
    jsonTest "TimeSpaceDecided" (Proxy :: Proxy TimeSpaceDecided)
  where
    jsonTest :: forall a. Arbitrary a => Eq a => EncodeJson a => DecodeJson a => String -> Proxy a -> _
    jsonTest name proxy = it name (liftEffect (quickCheck (jsonIso proxy)))


jsonIso :: forall a. Eq a => EncodeJson a => DecodeJson a => Proxy a -> a -> Boolean
jsonIso Proxy x = decodeJson (encodeJson x) == Right x
