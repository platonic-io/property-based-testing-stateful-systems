{-# LANGUAGE DerivingStrategies #-}

module Part05.Event
  (module Part05.Event) where

import Data.ByteString.Lazy (ByteString)

import Part05.Time
import Part05.StateMachine

------------------------------------------------------------------------

data Event
  = NetworkEventE NetworkEvent
  | TimerEventE   TimerEvent
  | CommandEventE CommandEvent
  deriving stock Show

data NetworkEvent = NetworkEvent NodeId (Input ByteString ByteString)
  deriving stock Show

data TimerEvent = TimerEvent NodeId TimerId Time
  deriving stock Show

getEventTime :: Event -> Time
getEventTime (TimerEventE   (TimerEvent   _nodeId _timerId time)) = time
getEventTime (NetworkEventE (NetworkEvent _nodeId input))         = getInputTime input
  where
    getInputTime :: Input request message -> Time
    getInputTime (ClientRequest   time _cid _req) = time
    getInputTime (InternalMessage time _nid _msg) = time
getEventTime CommandEventE {} = error "getEventTime: impossible"

data CommandEvent = Exit
  deriving stock Show

isExitCommand :: Event -> Bool
isExitCommand (CommandEventE Exit) = True
isExitCommand _otherwise           = False
