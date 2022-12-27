{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Part05.StateMachine
  (module Part05.StateMachine) where

import Data.ByteString.Lazy (ByteString)
import Data.Fixed
import Data.TreeDiff (ToExpr)
import System.Random

import Part05.Time

------------------------------------------------------------------------

newtype NodeId = NodeId { unNodeId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Read, Show, ToExpr)

newtype ClientId = ClientId { unClientId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Read, Show, ToExpr)

newtype TimerId = TimerId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

type SMStep state message response
  = state -> StdGen -> ([Output response message], state, StdGen)

-- The state machine type is a bit more complex what we've seen previously:
--
--   * input and output types are parameters so that applications with different
--     message types can written;
--   * inputs are split into client requests (synchronous) and internal messages
--     (asynchrous);
--   * a step in the SM can returns several outputs, this is useful for
--     broadcasting;
--   * outputs can also set and reset timers, which is necessary for
--     implementing retry logic;
--   * when the timers expire the event loop will call the SM's timeout handler
--     (`smTimeout`);
--   * in addition to the state we also thread through a seed, `StdGen`, so that
--     the SM can generate random numbers;
--   * there's also an initisation step (`smInit`) to set up the SM before it's
--     put to work.
data SM state request message response = SM
  { smState   :: state
  , smInit    :: SMStep state message response
  , smStep    :: Input request message -> SMStep state message response
  , smTimeout :: Time -> SMStep state message response
  -- smPredicate :: state -> [pred]
  -- smProcess :: pred -> state -> ([Output response message], state)
  }

data Input request message
  = ClientRequest Time ClientId request
  | InternalMessage Time NodeId message
  deriving stock Show

data Output response message
  = ClientResponse ClientId response
  | InternalMessageOut NodeId message
  | RegisterTimerSeconds TimerId Pico
  | ResetTimerSeconds TimerId Pico
  deriving stock (Eq, Show)

noInit :: SMStep state message response
noInit state stdgen = ([], state, stdgen)

noTimeouts :: Time -> SMStep state message response
noTimeouts _time state stdgen = ([], state, stdgen)

echoSM :: SM () ByteString ByteString ByteString
echoSM = SM
  { smState   = ()
  , smInit    = noInit
  , smStep    = step
  , smTimeout = noTimeouts
  }
  where
    step (ClientRequest _at cid req) () gen = ([ClientResponse cid req], (), gen )
    step _ _ _ = error "echoSM: impossible"
