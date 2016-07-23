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
                                  return $ if null updates then 0 else getLastId updates

processUpdate :: Update -> IO ()
processUpdate (Update _ msg Nothing) = do -- Process an Update that has a Message
  putStrLn $ "Received Message: " ++ show msg
  _ <- forkIO $ do
    _ <- reply $ fromJust msg
    return ()
  return ()
processUpdate (Update _ Nothing qry) = do  -- Process and Update that has an Inline Query
  putStrLn $ "Received InlineQuery: " ++ show qry
  return ()
processUpdate u = do
  putStrLn $ "Dont know how to process update: " ++ show u
  return ()

reply :: Message -> IO ()
reply msg = case fromMaybe "" (M.text msg) of 
                          "/herecomedatboi" -> sendMessage originChatId "Oh shit whaddup!"
                          "/пиздос" -> sendPhoto originChatId "pi.png"
                          _ -> sendMessage originChatId "Don't know what you're talking about"
  where originChatId = C.chatId $ M.chat msg
