{-# OPTIONS_GHC -Wall #-}

module Bot where

import           API.Requests
import qualified API.Types.Chat     as C
import qualified API.Types.Message  as M
import qualified API.Types.Response as R
import qualified API.Types.Update   as U
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

processUpdates :: Either String (R.Response [U.Update]) -> IO Integer
processUpdates (Left e) = fail e
processUpdates (Right response) = let updates = (fromJust $ R.result response) 
                                      getLastId = U.updateId . last
                                      in
                                  do 
                                  mapM_ processUpdate updates
                                  return $ if null updates then 0 else getLastId updates

processUpdate :: U.Update -> IO ()
processUpdate (U.Update _ msg Nothing) = do -- Process an Update that has a Message
  putStrLn $ "Received Message: " ++ show msg
  _ <- forkIO $ do
    _ <- replyToMessage $ fromJust msg
    return ()
  return ()
processUpdate (U.Update _ Nothing qry) = do  -- Process and Update that has an Inline Query
  putStrLn $ "Received InlineQuery: " ++ show qry
  return ()
processUpdate u = do
  putStrLn $ "Dont know how to process update: " ++ show u
  return ()

replyToMessage :: M.Message -> IO ()
replyToMessage msg = case fromMaybe "" (M.text msg) of 
                          "/herecomedatboi" -> sendMessage originChatId "Oh shit whaddup!"
                          "/пиздос" -> sendPhoto originChatId "pi.png"
                          unknown -> sendMessage originChatId ("Sorry, I don't know what you mean by '" ++ unknown ++ "'")
  where originChatId = C.chatId $ M.chat msg

--replyToInlineQuery :: InlineQuery -> IO ()
--replyToInlineQuery (InlineQuery _ _ q) = 
