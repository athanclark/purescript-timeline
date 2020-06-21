module Timeline.UI.Siblings where

import Timeline.UI.Index.Value (DecidedValue(..))
import Timeline.UI.Index.Span (DecidedSpan(..))
import Timeline.UI.Event (Event(..))
import Timeline.UI.TimeSpan (TimeSpan(..))
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan(..))
import Timeline.UI.Settings (Settings(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Default (class Default, def)
import Data.Argonaut (class EncodeJson, class DecodeJson, jsonParser, stringify, decodeJson, encodeJson)
import Test.QuickCheck (class Arbitrary)
import Effect (Effect)
import Effect.Exception (throw)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem, removeItem)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make, get, set, subscribeDiffLight)

newtype Siblings
  = Siblings (Array EventOrTimeSpan)

derive instance genericSiblings :: Generic Siblings _

derive newtype instance eqSiblings :: Eq Siblings

derive newtype instance showSiblings :: Show Siblings

derive newtype instance encodeJsonSiblings :: EncodeJson Siblings

derive newtype instance decodeJsonSiblings :: DecodeJson Siblings

derive newtype instance arbitrarySiblings :: Arbitrary Siblings

-- FIXME dummy data
instance defaultSiblings :: Default Siblings where
  def =
    Siblings
      [ EventOrTimeSpan $ Left (Event { name: "Event A", description: "baz", time: DecidedValueNumber 3.0 })
      , EventOrTimeSpan $ Left (Event { name: "Event B", description: "qux", time: DecidedValueNumber 3.5 })
      , EventOrTimeSpan $ Right (TimeSpan { name: "TimeSpan C", description: "bar", span: DecidedSpanNumber { start: 2.0, stop: 5.0 }, timeSpace: Nothing })
      , EventOrTimeSpan $ Right (TimeSpan { name: "TimeSpan D", description: "foo", span: DecidedSpanNumber { start: 1.0, stop: 4.0 }, timeSpace: Nothing })
      ]

localstorageSignalKey :: String
localstorageSignalKey = "localstorage"

localstorageKey :: String
localstorageKey = "Siblings"

-- TODO predicate from top-level index, and seek from selected time space.
newSiblingsSignal ::
  IxSignal ( read :: S.READ ) Settings ->
  Effect (IxSignal ( read :: S.READ, write :: S.WRITE ) Siblings)
newSiblingsSignal settingsSignal = do
  store <- window >>= localStorage
  mItem <- getItem localstorageKey store
  item <- case mItem of
    Nothing -> pure def
    Just s -> case jsonParser s >>= decodeJson of
      Left e -> throw $ "Couldn't parse Siblings: " <> e
      Right x -> pure x
  sig <- make item
  let
    handler x = do
      Settings { localCacheTilExport } <- get settingsSignal
      when localCacheTilExport
        $ setItem localstorageKey (stringify (encodeJson x)) store
  subscribeDiffLight localstorageSignalKey handler sig
  pure sig

clearSiblingsCache :: Effect Unit
clearSiblingsCache = do
  store <- window >>= localStorage
  removeItem localstorageKey store

setDefaultSiblings ::
  IxSignal ( write :: S.WRITE ) Siblings ->
  Effect Unit
setDefaultSiblings siblingsSignal = set def siblingsSignal
