module Timeline.Convert.UISets.EventsOrTimeSpans where

import Timeline.Convert.UISets.Events (getEventScoped)
import Timeline.Convert.UISets.TimeSpans (getTimeSpanScoped)
import Timeline.Convert.Errors (SynthesizeError)
import Timeline.UI.TimeSpan (TimeSpan)
import Timeline.UI.Event (Event)
import Timeline.UI.EventOrTimeSpan (EventOrTimeSpan(..), EventOrTimeSpanPoly(..))
import Timeline.ID.TimeSpan (TimeSpanID)
import Timeline.ID.Event (EventID)
import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Zeta.Types (READ) as S
import IxZeta.Map (IxSignalMap)
import IxZeta.Map (insert, get) as IxSignalMap

getEventOrTimeSpanScoped ::
  EventOrTimeSpanPoly EventID TimeSpanID ->
  IxSignalMap EventID ( read :: S.READ ) Event ->
  IxSignalMap TimeSpanID ( read :: S.READ ) TimeSpan ->
  Effect (Either SynthesizeError EventOrTimeSpan)
getEventOrTimeSpanScoped (EventOrTimeSpanPoly eOrTs) events timeSpans = case eOrTs of
  Left e -> map (EventOrTimeSpan <<< Left) <$> getEventScoped e events
  Right ts -> map (EventOrTimeSpan <<< Right) <$> getTimeSpanScoped ts timeSpans
