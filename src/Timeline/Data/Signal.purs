module Timeline.Data.Signal where




-- type TimeSpan rw index =
--   { startIndex  :: Signal rw index
--   , stopIndex   :: Signal rw index
--   , timeSpace   :: Signal rw (Maybe TimeSpaceDecided) -- FIXME recursive data? Index? if there's no lateral movement, then it can only exist or not exist. However, lateral
--                                                       -- movement is allowed, so precautions dictate policy.
--     -- non-essential
--   , name        :: Signal rw String
--   , description :: Signal rw String
--   , document    :: Signal rw String
--   }
