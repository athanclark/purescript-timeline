module Timeline.UI.TimeSpace where

import Timeline.UI.TimeScale (TimeScale)
import Data.UUID (UUID)

newtype TimeSpace
  = TimeSpace
  { title :: String
  , description :: String
  , timeScale :: TimeScale
  , eventSiblings :: Array UUID -- TODO manual field sorting
  , timeSpanSiblings :: Array UUID
  , timelines :: Array UUID
  , id :: UUID -- TODO trim the fat later
  }
