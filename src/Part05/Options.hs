module Part05.Options
  (module Part05.Options) where

import Part05.Deployment

------------------------------------------------------------------------

data Options = Options
  { oDeployment :: DeploymentMode
  -- , oTimerFreq :: Double -- Hz (cycles per second)
  -- oClientTimeoutNanos :: Int
  }
