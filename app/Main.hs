module Main where

import Control.Exception (assert)
import Control.Monad (unless)
import System.Environment
import System.Exit (die)

import Part05.ClientGenerator
import Part05.ErrorReporter
import Part05.EventLoop
import Part05.StateMachine
import Part05.Configuration
import Part05.Codec
import Part05.Debug
import Part05.Deployment
import Part05.Network
import Part05.Random
import Part05.History
import Part05.Time

import qualified Part04.LineariseWithFault as Part4

import qualified Part05.ViewstampReplication.Machine as VR
import Part05.ViewstampReplication.Test.ClientGenerator (vrGeneratorSchema)
import Part05.ViewstampReplication.Test.Model (smI, step, initModel, markFailure)

------------------------------------------------------------------------

runMany :: [Seed] -> (Seed -> IO Bool) -> IO ()
runMany origxs f = go (1 :: Int) origxs
  where
  go _i [] = return ()
  go i (x:xs) = do
    putStrLn $ "Running iteration: " ++ show i ++ " Seed " ++ show (unSeed x)
    res <- f x
    if res
      then go (succ i) xs
      else do
        die $ "Failed iteration: " ++ show i ++ " : Seed " ++ show (unSeed x)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--simulation"] -> do
      h <- newHistory
      _collector <- eventLoopSimulation (Seed 0) echoAgenda h [SomeCodecSM idCodec echoSM] NoGenerator
      _history <- readHistory h
      putStrLn "Can't print history yet, need Show/Pretty constraints for parameters..."
    ["vr", "--simulation", seed, fp] -> do
      h <- newHistory
      let
        nodes = map NodeId [0..4]
        delta = 15 -- time for primary to do re-broadcast
        vrSM me = VR.vrSM (filter (/= me) nodes) me delta [] smI
        printItem label prefix thing =
          putStrLn $ "\x1b[33m" <> label <> ":\x1b[0m " <> prefix <> show thing
        printE (HistEvent' d (HistEvent n bs inp as msgs)) = do
          putStrLn "\n\x1b[32mNew Entry\x1b[0m"
          printItem "Node" " " n
          printItem "State before" "\n" bs
          printItem "Input" (case d of { DidDrop -> "\x1b[31m[DROPPED]\x1b[0m "; DidArrive -> " "}) inp
          printItem "State after" "\n" as
          printItem "Sent messages" "" ""
          mapM_ (\x -> putStrLn $ "  " <> show x) msgs
        fs = FailureSpec (NetworkFaults 0.15)
        seed' = read seed
        endTime = addTimeSeconds 3600 epoch
      collector <- eventLoopFaultySimulation (Seed seed') (VR.agenda endTime) h fs
        [ SomeCodecSM VR.vrCodec (vrSM me) | me <- nodes] vrGeneratorSchema
      history <- readHistory h
      mapM_ printE history
      -- let's print the errors again so they are easier to see.
      reportedErrors <- readFromCollector collector
      unless (null reportedErrors) (putStrLn "")
      mapM_ putStrLn reportedErrors
      writeDebugFile fp history
      let bbHistory = markFailure (blackboxFailHistory (fmap heEvent history))
      assert (Part4.linearise step initModel bbHistory) (return ())
    ["vr", "--simulation-explore"] -> do
      seeds <- generateSeeds 10
      runMany seeds $ \ seed -> do
        h <- newHistory
        let
          nodes = map NodeId [0..4]
          delta = 15 -- time for primary to do re-broadcast
          vrSM me = VR.vrSM (filter (/= me) nodes) me delta [] smI
          fs = FailureSpec (NetworkFaults 0.15)
          endTime = addTimeSeconds 3600 epoch
        collector <- eventLoopFaultySimulation seed (VR.agenda endTime) h fs
          [ SomeCodecSM VR.vrCodec (vrSM me) | me <- nodes] vrGeneratorSchema
        history <- readHistory h
        -- let's print the errors again so they are easier to see.
        reportedErrors <- readFromCollector collector
        unless (null reportedErrors) (putStrLn "")
        mapM_ putStrLn reportedErrors
        let bbHistory = markFailure (blackboxFailHistory (fmap heEvent history))
            isValid = Part4.linearise step initModel bbHistory
        print bbHistory
        return (null reportedErrors && isValid)
    _otherwise       -> eventLoopProduction [SomeCodecSM idCodec echoSM]
