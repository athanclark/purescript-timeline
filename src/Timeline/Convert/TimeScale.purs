module Timeline.Convert.TimeScale where

import Timeline.Time.Unit (DecidedUnit)
import Timeline.Time.Value (DecidedValue)
import Timeline.Time.MaybeLimit (makeDecidedMaybeLimitViaMaybeLimit, unmakeDecidedMaybeLimitWithMaybeLimit)
import Timeline.UI.TimeScale (TimeScale(..)) as UI
import Timeline.Data.TimeScale (TimeScale(..)) as Data
import Data.Maybe (Maybe(..))

dataToUi :: DecidedUnit -> Data.TimeScale DecidedValue -> Maybe UI.TimeScale
dataToUi unit (Data.TimeScale x) = case makeDecidedMaybeLimitViaMaybeLimit unit x.limit of
  Nothing -> Nothing
  Just limit -> Just (UI.TimeScale x { limit = limit })

uiToData :: UI.TimeScale -> { timeScale :: Data.TimeScale DecidedValue, unit :: DecidedUnit }
uiToData (UI.TimeScale x) = case unmakeDecidedMaybeLimitWithMaybeLimit x.limit of
  { limit, unit } -> { timeScale: Data.TimeScale x { limit = limit }, unit }
