module Timeline.UI.Timelines where

import Timeline.UI.Timeline (Timeline(..))
import Timeline.UI.Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Default (class Default, def)
import Data.Argonaut (class EncodeJson, class DecodeJson, jsonParser, decodeJson, stringify, encodeJson)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Exception (throw)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Test.QuickCheck (class Arbitrary)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, get, set, subscribeDiffLight)

-- | All positioning information is ad-hoc, therefore a "sorted IxDemiSet with position information"
-- | would be equivalent to an array.
newtype Timelines
  = Timelines (Array Timeline)

derive instance genericTimelines :: Generic Timelines _

derive newtype instance eqTimelines :: Eq Timelines

derive newtype instance showTimelines :: Show Timelines

derive newtype instance encodeJsonTimelines :: EncodeJson Timelines

derive newtype instance decodeJsonTimelines :: DecodeJson Timelines

derive newtype instance arbitraryTimelines :: Arbitrary Timelines

-- FIXME dummy data
instance defaultTimelines :: Default Timelines where
  def =
    Timelines
      [ Timeline { name: "Timeline A", description: "" }
      , Timeline { name: "Timeline B", description: "" }
      , Timeline { name: "Timeline C", description: "" }
      , Timeline { name: "Timeline D", description: "" }
      , Timeline { name: "Timeline E", description: "" }
      , Timeline { name: "Timeline F", description: "" }
      ]

localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "Timelines"

-- TODO predicate from top-level index, and seek from selected time space.
newTimelinesSignal ::
  IxSignal ( read :: S.READ ) Settings ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) Timelines)
newTimelinesSignal settingsSignal = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure def
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse Timelines: " <> e
      Right x -> pure x
  sig <- make item
  let
    handler x = do
      Settings { localCacheTilExport } <- get settingsSignal
      when localCacheTilExport
        $ setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

clearTimelinesCache :: Effect Unit
clearTimelinesCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store

setDefaultTimelines ::
  IxSignal ( write :: S.WRITE ) Timelines ->
  Effect Unit
setDefaultTimelines timelinesSignal = set def timelinesSignal
