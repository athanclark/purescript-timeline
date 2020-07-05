module Timeline.UI.TimeSpace where

import Timeline.UI.TimeSpace.TimeScale (TimeScale)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpanPoly(..))
import Timeline.UI.Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Bifunctor (bimap)
import Data.Traversable (traverse)
import Data.Default (class Default, def)
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, genUUID) as UUID
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
  , fail
  , jsonParser
  , stringify
  )
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, set, get, make, subscribeDiffLight)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, arrayOf)
import Test.QuickCheck.UTF8String (genString)

newtype TimeSpace
  = TimeSpace
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , siblings :: Array (EventOrTimeSpanPoly UUID UUID) -- TODO manual field sorting
  , timelines :: Array UUID
  , id :: UUID -- TODO trim the fat later
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
      := map (bimap UUID.toString UUID.toString) siblings
      ~> "timelines"
      := map UUID.toString timelines
      ~> "id"
      := UUID.toString id
      ~> jsonEmptyObject

instance decodeJsonTimeSpace :: DecodeJson TimeSpace where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    let
      getUUID s = case UUID.parseUUID s of
        Nothing -> fail $ "Couldn't parse UUID: " <> s
        Just x -> pure x

      getIds (EventOrTimeSpanPoly eOrTs) = case eOrTs of
        Left s -> EventOrTimeSpanPoly <<< Left <$> getUUID s
        Right s -> EventOrTimeSpanPoly <<< Right <$> getUUID s
    timeScale <- o .: "timeScale"
    siblings <- o .: "siblings" >>= traverse getIds
    timelines <- o .: "timelines" >>= traverse getUUID
    id <- o .: "id" >>= getUUID
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })

instance arbitraryEvent :: Arbitrary TimeSpace where
  arbitrary = do
    title <- genString
    description <- genString
    timeScale <- arbitrary
    let
      genId = pure (unsafePerformEffect UUID.genUUID)
    siblings <- arrayOf $ EventOrTimeSpanPoly <$> oneOf (NonEmpty (Left <$> genId) [ Right <$> genId ])
    timelines <- arrayOf genId
    id <- genId
    pure (TimeSpace { title, description, timeScale, siblings, timelines, id })

instance defaultTimeSpace :: Default TimeSpace where
  def =
    TimeSpace
      { title: "TimeSpace Name"
      , description: ""
      , timeScale: def
      , siblings: []
      , timelines: []
      , id: unsafePerformEffect UUID.genUUID
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
    Nothing -> pure def
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

setDefaultTimeSpace ::
  IxSignal ( write :: S.WRITE ) TimeSpace ->
  Effect Unit
setDefaultTimeSpace timeSpaceSignal = set def timeSpaceSignal
