module Timeline.Convert.UISets.Timelines where

import Timeline.Convert.Errors (PopulateError(TimelineExists), SynthesizeError(TimelineDoesntExist))
import Timeline.UI.Timeline (Timeline(..))
import Timeline.ID.Timeline (TimelineID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Zeta.Types (READ, WRITE) as S
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (insert, get) as IxSignalMap

addTimelineScoped :: Timeline -> IxSignalMap TimelineID ( write :: S.WRITE ) Timeline -> Effect (Either PopulateError Unit)
addTimelineScoped x@(Timeline { id }) timelines = do
  succeeded <- IxSignalMap.insert id x timelines
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimelineExists x)

getTimelineScoped :: TimelineID -> IxSignalMap TimelineID ( read :: S.READ ) Timeline -> Effect (Either SynthesizeError Timeline)
getTimelineScoped id timelines = do
  mTimeline <- IxSignalMap.get id timelines
  pure
    $ case mTimeline of
        Nothing -> Left (TimelineDoesntExist id)
        Just timeline -> Right timeline
