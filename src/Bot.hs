{-# OPTIONS_GHC -Wall #-}

module Bot where

import           API.Requests
import           API.Types.Chat     as C
import           API.Types.Message  as M
import           API.Types.Response as R
import           API.Types.Update   as U
import           Control.Concurrent (forkIO, threadDelay)
import           Data.Maybe

startGetUpdatesLoopWithDelay :: Float -> IO ()
startGetUpdatesLoopWithDelay delay = do
  initialUpdates <- getUpdates
  unpackedUpdates <- unpackUpdates initialUpdates
  lastUpdateId <- getLastUpdateId unpackedUpdates
  mapM_ processUpdate unpackedUpdates
  getUpdatesLoop delay lastUpdateId

getUpdatesLoop :: Float -> Integer -> IO ()
getUpdatesLoop delay lastReceivedUpdateId = do
  updates <- getUpdatesWithId (lastReceivedUpdateId + 1)
  unpackedUpdates <- unpackUpdates updates
  lastUpdateId <- getLastUpdateId unpackedUpdates
  mapM_ processUpdate unpackedUpdates
  threadDelay $ truncate (1000000 * delay)
  getUpdatesLoop delay lastUpdateId

unpackUpdates :: Either String (Response [Update]) -> IO [Update]
unpackUpdates (Left e) = do
  putStrLn ("Can't process updates: " ++ e)
  return []
unpackUpdates (Right r) = return (fromJust $ R.result r)

getLastUpdateId :: [Update] -> IO Integer
getLastUpdateId [] = return 0
getLastUpdateId xs = return $ (U.updateId . last) xs

processUpdate :: Update -> IO ()
processUpdate u = do
  _ <- forkIO $ do
    _ <- sendMessage originChatId (getMessage (fromMaybe "" (M.text msg)))
    return ()
  return ()
  where msg = fromJust $ U.message u
        originChatId = C.chatId $ M.chat msg

getMessage :: String -> String
getMessage s = case s of
                    _ -> "You said: " ++ s

