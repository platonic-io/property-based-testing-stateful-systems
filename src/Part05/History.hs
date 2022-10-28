{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Part05.History (module Part05.History) where

import Control.Concurrent.STM
import Data.TreeDiff (ToExpr)
import Data.Typeable

import Part02ConcurrentSMTesting (History', Operation'(..), Pid(Pid))
import qualified Part02ConcurrentSMTesting as Part2
import qualified Part04.LineariseWithFault as Part4
import Part05.StateMachine

------------------------------------------------------------------------

newtype History = History (TQueue HistEvent')

data Dropped = DidDrop | DidArrive

data HistEvent' = HistEvent'
  { heDropped :: Dropped
  , heEvent :: HistEvent
  }

data HistEvent = forall state req msg resp.
  ( Show state, Show req, Show msg, Show resp
  , ToExpr state
  , Typeable req, Typeable resp
  ) => HistEvent NodeId state (Input req msg) state [Output resp msg]
deriving stock instance Show HistEvent

newHistory :: IO History
newHistory = do
  q <- newTQueueIO
  return (History q)

appendHistory :: History -> Dropped -> HistEvent -> IO ()
appendHistory (History q) dropped ev = atomically (writeTQueue q (HistEvent' dropped ev))

readHistory :: History -> IO [HistEvent']
readHistory (History q) = atomically (flushTQueue q)

------------------------------------------------------------------------

blackboxHistory :: forall req resp. (Typeable req, Typeable resp)
                => [HistEvent] -> History' req resp
blackboxHistory = Part2.History . go []
  where
    go :: [Operation' req resp] -> [HistEvent] -> [Operation' req resp]
    go acc [] = reverse acc
    go acc (HistEvent _nodeId _state input _state' outputs : evs) =
      go (foldMap clientResponse outputs ++ clientRequest input ++ acc) evs

    clientRequest :: Typeable req' => Input req' msg -> [Operation' req resp]
    clientRequest (ClientRequest _time clientId req) = case cast req of
      Just req' -> [Invoke (clientIdToPid clientId) req']
      Nothing   -> error "Failed cast, can't add clientRequest"
    clientRequest _otherwise = []

    clientResponse :: Typeable resp' => Output resp' msg -> [Operation' req resp]
    clientResponse (ClientResponse clientId resp) = case cast resp of
      Just resp' -> [Ok (clientIdToPid clientId) resp']
      Nothing    -> error "Failed cast, can't add clientResponse"
    clientResponse _otherwise = []

    clientIdToPid :: ClientId -> Pid
    clientIdToPid (ClientId cid) = Pid cid

blackboxFailHistory :: forall req resp. (Typeable req, Typeable resp)
                    => [HistEvent] -> Part4.History' req resp
blackboxFailHistory he = case blackboxHistory he of
  Part2.History ops -> Part4.History (fmap go ops)
  where
    go (Part2.Invoke p c) = Part4.Invoke p c
    go (Part2.Ok p r) = Part4.Ok p r
