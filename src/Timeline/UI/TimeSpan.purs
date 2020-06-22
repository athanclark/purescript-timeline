module Timeline.UI.TimeSpan where

import Timeline.Time.Span (DecidedSpan)
import Prelude
import Data.Maybe (Maybe)
import Data.IxSet.Demi (Index)
import Data.Generic.Rep (class Generic)
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , decodeJson
  , (:=)
  , (~>)
  , jsonEmptyObject
  , (.:)
  )
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.UTF8String (genString)

-- | An inclusive span of time from `startIndex` to `stopIndex`.
-- |
-- | Defined over the user-level timescale `a`.
newtype TimeSpan
  = TimeSpan
  { name :: String
  , description :: String
  , span :: DecidedSpan
  , timeSpace :: Maybe Index
  }

derive instance genericTimeSpan :: Generic TimeSpan _

derive newtype instance eqTimeSpan :: Eq TimeSpan

derive newtype instance showTimeSpan :: Show TimeSpan

instance encodeJsonTimeSpan :: EncodeJson TimeSpan where
  encodeJson (TimeSpan { name, description, span, timeSpace }) =
    "name" := name
      ~> "description"
      := description
      ~> "span"
      := span
      ~> "timeSpace"
      := timeSpace
      ~> jsonEmptyObject

instance decodeJsonTimeSpan :: DecodeJson TimeSpan where
  decodeJson json = do
    o <- decodeJson json
    name <- o .: "name"
    description <- o .: "description"
    span <- o .: "span"
    timeSpace <- o .: "timeSpace"
    pure (TimeSpan { name, description, span, timeSpace })

instance arbitraryTimeSpan :: Arbitrary TimeSpan where
  arbitrary = do
    name <- genString
    description <- genString
    span <- arbitrary
    timeSpace <- arbitrary
    pure (TimeSpan { name, description, span, timeSpace })
