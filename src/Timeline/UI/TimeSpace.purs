module Timeline.UI.TimeSpace where

import Timeline.UI.TimeSpace.TimeScale (TimeScale)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly)
import Timeline.UI.Settings (Settings(..))
import Timeline.ID.TimeSpace (TimeSpaceID(..))
import Timeline.ID.Timeline (TimelineID)
import Timeline.ID.Event (EventID)
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Default (def)
import Data.UUID (genUUID) as UUID
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  , jsonParser
  , stringify
  )
import Effect (Effect)
import Effect.Exception (throw)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, set, get, make, subscribeDiffLight)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

newtype TimeSpace
  = TimeSpace
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , siblings :: Array (EventOrTimeSpanPoly EventID TimeSpanID) -- TODO manual field sorting
  , timelines :: Array TimelineID
  , id :: TimeSpaceID
  }

derive instance genericTimeSpace :: Generic TimeSpace _

derive newtype instance eqTimeSpace :: Eq TimeSpace

derive newtype instance showTimeSpace :: Show TimeSpace

instance encodeJsonTimeSpace :: EncodeJson TimeSpace where
  encodeJson (TimeSpace { title, description, timeScale, siblings, timelines, id }) =
    "title" := title
      ~> "description"
      := description
      ~> "timeScale"
      := timeScale
      ~> "siblings"
      := siblings
      ~> "timelines"
      := timelines
      ~> "id"
      := id
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: DecodeJson TimeSpace where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    timeScale <- o .: "timeScale"
    siblings <- o .: "siblings"
    timelines <- o .: "timelines"
    id <- o .: "id"
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })

instance arbitraryEvent :: Arbitrary TimeSpace where
  arbitrary = do
    title <- genString
    description <- genString
    timeScale <- arbitrary
    siblings <- arbitrary
    timelines <- arbitrary
    id <- arbitrary
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })

defaultTimeSpace :: Effect TimeSpace
defaultTimeSpace = newTimeSpace { title: "TimeSpace Name", description: "", timeScale: def }

newTimeSpace ::
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  } ->
  Effect TimeSpace
newTimeSpace { title, description, timeScale } = do
  id <- UUID.genUUID
  pure
    $ TimeSpace
        { title
        , description
        , timeScale
        , siblings: []
        , timelines: []
        , id: TimeSpaceID id
        }

localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "TimeSpace"

-- | Create the "viewed time space" signal on boot, where the initial
-- | time space is synthesized by `UISets`.
newTimeSpaceSignal ::
  { settingsSignal :: IxSignal ( read :: S.READ ) Settings
  , initialTimeSpace :: TimeSpace
  } ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpace)
newTimeSpaceSignal { settingsSignal, initialTimeSpace } = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure initialTimeSpace
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse TimeSpace: " <> e
      Right x -> pure x
  sig <- make item
  let
    handler x = do
      Settings { localCacheTilExport } <- get settingsSignal
      when localCacheTilExport
        $ setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

clearTimeSpaceCache :: Effect Unit
clearTimeSpaceCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store

setNewDocumentTimeSpace ::
  IxSignal ( write :: S.WRITE ) TimeSpace ->
  Effect Unit
setNewDocumentTimeSpace timeSpaceSignal = do
  timeSpace <- defaultTimeSpace
  set timeSpace timeSpaceSignal
