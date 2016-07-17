{-# OPTIONS_GHC -Wall #-}

module Bot where

import Control.Concurrent
import Control.Monad (forever, mapM_)
import API.Requests
import API.Types
import Data.Maybe

startGetUpdatesLoopWithDelay :: Int -> IO () 
startGetUpdatesLoopWithDelay delay = forever $ do 
  lastUpdateID <- newEmptyMVar
  updates <- getUpdates
  process updates
  threadDelay (1000000 * delay)
  where process (Left e) = putStrLn $ "Failed to fetch updates: " ++ e 
        process (Right r) = print $ map updateId $ fromJust $ result r

--processUpdates :: [Update] -> IO ()
--processUpdates = mapM_ processUpdate
--
--processUpdate 
