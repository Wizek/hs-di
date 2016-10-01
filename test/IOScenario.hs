module IOScenario where

import DI
import Data.Time

injMG
startupTime :: UTCTime
startupTimeI = do
  print "startupTime init"
  getCurrentTime

injG
-- startupTimeString :: String
startupTimeStringI startupTime = show (startupTime :: UTCTime)

injG
-- startupTimeString :: String
startupTimeStringBBBI startupTime = show (startupTime :: UTCTime) ++ "-BBB"

injG
xxxI
  a @ (InjIO startupTimeString)
  b @ (InjIO startupTimeStringBBB)
  (InjIO x @ startupTimeStringBBB)
  = a ++ b


asdasdI x y = undefined

asdasd2I (Inj x) y = undefined
-- startupTimeStringIOxxxI startupTime = startupTime >>= show .> return

-- startupTimeString =
--   let (startupTimeStringI, startupTimeI) = startupTimeStringT
--   in  startupTimeA >>= \startupTime -> startupTimeStringI startupTime

-- startupTimeString' =
--   let (startupTimeStringI, (startupTimeI, getCurrentTimeA)) = startupTimeStringT
--   in  (startupTimeI getCurrentTimeA) >>= \startupTime -> startupTimeStringI startupTime

-- a' =
--   let (aI, (bI, c)) = aT
--   in let b = bI c in let a = aI b in a

-- a'' =
--   let (aI, (bI, c)) = aT
--   in bI c >>= \b-> let a = aI b in a
