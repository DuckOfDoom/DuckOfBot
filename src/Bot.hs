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
  response <- getUpdates
  lastUpdateId <- processUpdates response 
  getUpdatesLoop delay lastUpdateId
    where getUpdatesLoop d lastId = do
            response <- getUpdatesWithId (lastId + 1)
            newId <- processUpdates response
            threadDelay $ truncate (1000000 * delay)
            getUpdatesLoop d newId

processUpdates :: Either String (Response [Update]) -> IO Integer
processUpdates (Left e) = fail e
processUpdates (Right response) = let updates = (fromJust $ R.result response) 
                                      getLastId = U.updateId . last
                                      in
                                  do 
                                  mapM_ processUpdate updates
                                  return $ if (null updates) then 0 else getLastId updates

processUpdate :: Update -> IO ()
processUpdate u = do
  putStrLn $ "Received Update: " ++ show u
  _ <- forkIO $ do
    _ <- reply $ fromJust $ U.message u
    return ()
  return ()

reply :: Message -> IO ()
reply msg = case (fromMaybe "" (M.text msg)) of 
                          "/herecomedatboi" -> sendMessage originChatId "Oh shit whaddup!"
                          "/pi" -> sendPhoto originChatId "curse.png"
                          _ -> sendMessage originChatId "Don't know what you're talking about"
  where originChatId = C.chatId $ M.chat msg
