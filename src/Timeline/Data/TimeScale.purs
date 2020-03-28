module Timeline.Data.TimeScale where


newtype TimeScale aux a = TimeScale
  { beginIndex :: a
  , endIndex   :: a
  | aux }

