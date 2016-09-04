{-# language NoMonomorphismRestriction #-}

module Main10RealCode where

import Main10TH

import System.IO
import Data.IORef
import Control.Monad
import Data.Time

putStrLnD = Dep "putStrLn" []
getCurrentTimeD = Dep "getCurrentTime" []

makeTimerD = Dep "makeTimer" [putStrLnD, getCurrentTimeD]
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