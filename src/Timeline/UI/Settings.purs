{-
OpenChronology - an application for graphing and visualizing timelines.
Copyright (C) 2020  Athan Clark

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published
by the Free Software Foundation version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
module Timeline.UI.Settings where

{-|

This module defines the `Settings` record, and how to obtain a signal for it,
which gets its information from LocalStorage.

-}
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
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem)
import Effect (Effect)
import Effect.Console (warn)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, subscribeDiffLight)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | The `Settings` record, which is JSON encodable so it can be stored.
newtype Settings
  = Settings
  { isEditable :: Boolean -- ^ `true` when opening a new timeline, `false` when loading one
  , isSearchable :: Boolean -- ^ `true` shows side panels
  , localCacheTilExport :: Boolean -- ^ `true` by default - store local changes until export
  }

derive instance genericSettings :: Generic Settings _

derive newtype instance eqSettings :: Eq Settings

derive newtype instance showSettings :: Show Settings

instance arbitrarySettings :: Arbitrary Settings where
  arbitrary = do
    isEditable <- arbitrary
    isSearchable <- arbitrary
    localCacheTilExport <- arbitrary
    pure (Settings { isEditable, isSearchable, localCacheTilExport })

instance encodeJsonSettings :: EncodeJson Settings where
  encodeJson (Settings { isEditable, isSearchable, localCacheTilExport }) =
    "isEditable" := isEditable
      ~> "isSearchable"
      := isSearchable
      ~> "localCacheTilExport"
      := localCacheTilExport
      ~> jsonEmptyObject

instance decodeJsonSettings :: DecodeJson Settings where
  decodeJson json = do
    o <- decodeJson json
    isEditable <- o .: "isEditable"
    isSearchable <- o .: "isSearchable"
    localCacheTilExport <- o .: "localCacheTilExport"
    pure (Settings { isEditable, isSearchable, localCacheTilExport })

-- | The key to be used by the `Handler` that listens to the signal for changes
localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

-- | The key to be used in LocalStorage when storing or looking up data
localstorageKey :: String
localstorageKey = "Settings"

-- | Create a `Signal` which updates the LocalStorage record whenever the settings change.
-- | The initial argument makes sure the interface isn't in edit mode when opening a shared link.
newSettingsSignal ::
  { wasOpenedByShareLink :: Boolean
  } ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) Settings)
newSettingsSignal { wasOpenedByShareLink } = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  let
    isEditable = not wasOpenedByShareLink

    def =
      Settings
        { isEditable
        , isSearchable: isEditable
        , localCacheTilExport: true
        }
  item <- case mItem of
    Nothing -> pure def
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> do
        warn $ "Couldn't parse Settings: " <> e
        pure def
      Right x -> pure x
  sig <- make item
  -- Always store settings
  let
    handler x = setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

-- | What the settings value should be for new users, with no link opened.
defaultSettings :: Settings
defaultSettings = Settings { isEditable: true, isSearchable: true, localCacheTilExport: true }
