module PartTest where

import Part03.Service (Bug(NoBug))
import qualified Part04FaultInjection as Part04

------------------------------------------------------------------------

unit_part04SeqIntegrationTests :: IO ()
unit_part04SeqIntegrationTests = Part04.unit_seqIntegrationTests NoBug
