module IOScenario where

import DI
import Data.Time

injMG
startupTimeI = getCurrentTime

injG
startupTimeStringI startupTime = show startupTime

startupTimeString =
  let (startupTimeStringI, startupTimeI) = startupTimeStringT
  in  startupTimeA >>= \startupTime -> startupTimeStringI startupTime

startupTimeString' =
  let (startupTimeStringI, (startupTimeI, getCurrentTimeA)) = startupTimeStringT
  in  (startupTimeI getCurrentTimeA) >>= \startupTime -> startupTimeStringI startupTime

a' =
  let (aI, (bI, c)) = aT
  in let b = bI c in let a = aI b in a

a'' =
  let (aI, (bI, c)) = aT
  in bI c >>= \b-> let a = aI b in a
