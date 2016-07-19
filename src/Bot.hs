{-# OPTIONS_GHC -Wall #-}

module Bot where

import           API.Requests
import           API.Types.Chat as C
import           API.Types.Message as M
import           API.Types.Update as U
import           API.Types.Response as R
import           API.Types (Update)
import           API.Args
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad      (forever)
import           Data.Maybe

startGetUpdatesLoopWithDelay :: Int -> IO ()
startGetUpdatesLoopWithDelay delay = 
    getUpdates' 0  -- Starting with update #1
  where getUpdates' id = do
          response <- getUpdatesWithId id 
          processUpdates response
          threadDelay (1000000 * delay)
          getUpdates' 0

processUpdates :: Either String (Response [Update]) -> IO ()
processUpdates (Left e) = putStrLn $ "Can't process updates: " ++ e
processUpdates (Right r) = mapM_ processUpdate $ fromJust $ R.result r

processUpdate :: Update -> IO ()
processUpdate u = do 
  threadId <- forkIO $ do 
    response <- sendMessage chatId "I hear ya"
    return ()
  return ()
  where msg = fromJust $ U.message u
        chatId = show $ C.chatId $ M.chat msg
