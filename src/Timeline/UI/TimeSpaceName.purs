module Timeline.UI.TimeSpaceName where

import Timeline.UI.Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , (:=)
  , (.:)
  , (~>)
  , jsonEmptyObject
  , stringify
  , jsonParser
  )
import Data.Default (class Default, def)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Effect (Effect)
import Effect.Exception (throw)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, get, set, subscribeDiffLight)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | Represents both the filename and the timeline's presented name
newtype TimeSpaceName
  = TimeSpaceName
  { title :: String
  , description :: String
  }

derive instance genericTimeSpaceName :: Generic TimeSpaceName _

derive newtype instance eqTimeSpaceName :: Eq TimeSpaceName

derive newtype instance showTimeSpaceName :: Show TimeSpaceName

instance encodeJsonTimeSpaceName :: EncodeJson TimeSpaceName where
  encodeJson (TimeSpaceName { title, description }) =
    "title" := title
      ~> "description"
      := description
      ~> jsonEmptyObject

instance decodeJsonTimeSpaceName :: DecodeJson TimeSpaceName where
  decodeJson json = do
    o <- decodeJson json
    title <- o .: "title"
    description <- o .: "description"
    pure (TimeSpaceName { title, description })

instance arbitraryTimeSpaceName :: Arbitrary TimeSpaceName where
  arbitrary = do
    title <- genString
    description <- genString
    pure (TimeSpaceName { title, description })

instance defaultTimeSpaceName :: Default TimeSpaceName where
  def =
    TimeSpaceName
      { title: "TimeSpace Name"
      , description: ""
      }

localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "TimeSpaceName"

-- FIXME need to differentiate root from children; and whether a single node being a signal is even valid
-- | Chosen timeline name on boot, disregarding the shared signal
newTimeSpaceNameSignal ::
  IxSignal ( read :: S.READ ) Settings ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) TimeSpaceName)
newTimeSpaceNameSignal settingsSignal = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure def
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse TimeSpaceName: " <> e
      Right x -> pure x
  sig <- make item
  let
    handler x = do
      Settings { localCacheTilExport } <- get settingsSignal
      when localCacheTilExport
        $ setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

clearTimeSpaceNameCache :: Effect Unit
clearTimeSpaceNameCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store

setDefaultTimeSpaceName ::
  IxSignal ( write :: S.WRITE ) TimeSpaceName ->
  Effect Unit
setDefaultTimeSpaceName timeSpaceNameSignal = set def timeSpaceNameSignal
