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
