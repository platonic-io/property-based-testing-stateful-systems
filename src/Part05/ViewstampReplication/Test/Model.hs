module Part05.ViewstampReplication.Test.Model
  (module Part05.ViewstampReplication.Test.Model)
  where

import qualified Part04.LineariseWithFault as Part4

import Part05.ViewstampReplication.Message
import Part05.ViewstampReplication.State (ReplicatedStateMachine(..))

------------------------------------------------------------------------

type Model = [String]
type Command = VRRequest String
type Response = VRResponse [String]

step :: Model -> Command -> (Model, Response)
step xs (VRRequest op rn) = (op:xs, VRReply 0 rn (op:xs))

initModel :: [String]
initModel = []

smI :: ReplicatedStateMachine [String] String [String]
smI = ReplicatedStateMachine $ \ s o -> (o:s, o:s)

---

markFailure :: Part4.History' Command Response -> Part4.History' Command Response
markFailure (Part4.History ops) = Part4.History (finishClients [] $ map go ops)
  where
    go i@Part4.Invoke{} = i
    go f@Part4.Fail{} = f
    go (Part4.Ok p VROnlyOneInflightAllowed{}) = Part4.Fail p Part4.FAIL
    go o@Part4.Ok{} = o

    remove x = filter (/= x)

    finishClients ps [] = [ Part4.Fail p Part4.FAIL | p <- ps]
    finishClients ps (op:h) = case op of
      Part4.Invoke p _ -> op : finishClients (p:ps) h
      Part4.Ok p _ -> op : finishClients (remove p ps) h
      Part4.Fail p _ -> op : finishClients (remove p ps) h
