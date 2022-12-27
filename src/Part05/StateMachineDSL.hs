{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Part05.StateMachineDSL
  ( module Part05.StateMachineDSL
  , module Lens.Micro.Platform
  )
  where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Fixed
import Lens.Micro.Platform
import qualified System.Random as Random
import System.Random (StdGen, mkStdGen)

import Part05.StateMachine
import Part05.Time

------------------------------------------------------------------------

-- The DSL allows us to use do syntax, do `send`s or register timers anywhere
-- rather than return a list outputs, as well as add pre-conditions via guards
-- and do early returns.
type SMM s msg resp a =
  ContT Guard (StateT s (StateT StdGen (Writer [Output resp msg]))) a

data Guard = Keep | Drop

runSMM :: SMM s msg resp () -> SMStep s msg resp
runSMM m s gen =
  case runWriter (runStateT (runStateT (runContT m (const (return Keep))) s) gen) of
    (((Keep,  s'),  gen'),  outputs) -> (outputs, s', gen')
    (((Drop, _s'), _gen'), _outputs) -> ([], s, gen)

send :: NodeId -> msg -> SMM s msg resp ()
send nid msg = lift (lift (lift (tell [InternalMessageOut nid msg])))

respond :: ClientId -> resp -> SMM s msg resp ()
respond cid resp = lift (lift (lift (tell [ClientResponse cid resp])))

registerTimerSeconds :: TimerId -> Pico -> SMM s msg resp ()
registerTimerSeconds timerId secs = lift (lift (lift (tell [RegisterTimerSeconds timerId secs])))

resetTimerSeconds :: TimerId -> Pico -> SMM s msg resp ()
resetTimerSeconds timerId secs = lift (lift (lift (tell [ResetTimerSeconds timerId secs])))

ereturn :: SMM s msg resp a
ereturn = ContT (const (return Keep))

guard :: Bool -> SMM s msg resp ()
guard True  = return ()
guard False = ContT (const (return Drop))

guardM :: SMM s msg resp Bool -> SMM s msg resp ()
guardM m = do
  b <- m
  guard b

random :: SMM s msg resp Int
random = do
  g <- lift (lift get)
  let (i, g') = Random.random g
  lift (lift (put g'))
  return i

data ExampleState = ExampleState
  { _esInt :: Int
  }
  deriving stock (Eq, Show)

makeLenses ''ExampleState

initExState :: ExampleState
initExState = ExampleState 0

data Req = Req
  deriving stock (Eq, Show)

data Msg = Msg
  deriving stock (Eq, Show)

data Resp = Resp Int
  deriving stock (Eq, Show)

example :: Input Req Msg -> SMM ExampleState Msg Resp ()
example (ClientRequest _at cid _req) = do
  esInt .= 1
  esInt += 2
  esInt += 3
  s <- use esInt
  respond cid (Resp s)
example _otherwise = return ()

t :: Bool
t = runSMM (example (ClientRequest epoch (ClientId 0) Req)) initExState (mkStdGen 0)
    == ([ClientResponse (ClientId 0) (Resp 6)], ExampleState {_esInt = 6}, (mkStdGen 1))

sm :: SM ExampleState Req Msg Resp
sm = SM initExState noInit (\i s g -> runSMM (example i) s g) noTimeouts
