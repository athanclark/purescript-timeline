module Timeline.Convert.UISets.Events where

import Timeline.Convert.Errors (PopulateError(EventExists), SynthesizeError(EventDoesntExist))
import Timeline.UI.Event (Event(..))
import Timeline.ID.Event (EventID)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Zeta.Types (READ, WRITE) as S
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (insert, get) as IxSignalMap

addEventScoped :: Event -> IxSignalMap EventID ( write :: S.WRITE ) Event -> Effect (Either PopulateError Unit)
addEventScoped x@(Event { id }) events = do
  succeeded <- IxSignalMap.insert id x events
  pure
    $ if succeeded then
        Right unit
      else
        Left (EventExists x)

getEventScoped :: EventID -> IxSignalMap EventID ( read :: S.READ ) Event -> Effect (Either SynthesizeError Event)
getEventScoped id events = do
  mEvent <- IxSignalMap.get id events
  pure
    $ case mEvent of
        Nothing -> Left (EventDoesntExist id)
        Just event -> Right event
