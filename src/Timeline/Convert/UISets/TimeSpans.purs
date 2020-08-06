module Timeline.Convert.UISets.TimeSpans where

import Timeline.Convert.Errors (PopulateError(TimeSpanExists), SynthesizeError(TimeSpanDoesntExist))
import Timeline.UI.TimeSpan (TimeSpan(..))
import Timeline.ID.TimeSpan (TimeSpanID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Zeta.Types (READ, WRITE) as S
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (insert, get) as IxSignalMap

addTimeSpanScoped :: TimeSpan -> IxSignalMap TimeSpanID ( write :: S.WRITE ) TimeSpan -> Effect (Either PopulateError Unit)
addTimeSpanScoped x@(TimeSpan { id }) timeSpans = do
  succeeded <- IxSignalMap.insert id x timeSpans
  pure
    $ if succeeded then
        Right unit
      else
        Left (TimeSpanExists x)

getTimeSpanScoped :: TimeSpanID -> IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan -> Effect (Either SynthesizeError TimeSpan)
getTimeSpanScoped id timeSpans = do
  mTimeSpan <- IxSignalMap.get id timeSpans
  pure
    $ case mTimeSpan of
        Nothing -> Left (TimeSpanDoesntExist id)
        Just timeSpan -> Right timeSpan
