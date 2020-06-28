module Timeline.Convert.Event where

import Timeline.Time.Value (DecidedValue)
import Timeline.UI.Event (Event(..)) as UI
import Timeline.Data.Event (Event(..)) as Data

dataToUi :: Data.Event DecidedValue -> UI.Event
dataToUi (Data.Event x) = UI.Event x

uiToData :: UI.Event -> Data.Event DecidedValue
uiToData (UI.Event x) = Data.Event x
