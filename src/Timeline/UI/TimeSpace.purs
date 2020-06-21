module Timeline.UI.TimeSpace where

newtype TimeSpace a
  = TimeSpace
  { title :: String
  , description :: String
  -- TODO document :: String
  }
