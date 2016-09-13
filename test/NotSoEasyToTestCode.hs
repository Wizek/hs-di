{-# language NoMonomorphismRestriction #-}
{-# language TemplateHaskell #-}

module NotSoEasyToTestCode where

import DI

import System.IO
import Data.IORef
import Control.Monad
import Data.Time


injLeaf "putStrLn"
injLeaf "getCurrentTime"

inj
makeTimer putStrLn getCurrentTime = liftIO $ do
  prevTime <- newIORef Nothing
  return $ liftIO $ do
    pTime <- readIORef prevTime
    time <- getCurrentTime
    writeIORef prevTime $ Just time
    case pTime of
      Nothing -> putStrLn $ show time
      Just a  -> putStrLn $ show time ++ ", diff: " ++ (show $ diffUTCTime time a)

-- Consider importing Shelly to make this even more realistic
liftIO = id

-- Dep "makeTimer" [Dep "putStrLn" [], Dep "getCurrentTime" []]
-- a = (makeTimer, putStrLn, getCurrentTime)
-- makeTimer' = let (f, a, b) = a in f a b

-- Dep "makeTimer" [Dep "putStrLn" [], Dep "getCurrentTime" [], Dep "foo" [Dep "bar" []]]
-- a = (makeTimer, putStrLn, getCurrentTime, (foo, bar))
-- makeTimer' = let (f, a, b, (g, d)) = a in f a b (g d)

