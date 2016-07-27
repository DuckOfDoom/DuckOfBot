{-# OPTIONS_GHC -Wall #-}

module Bot where

import           Data.Aeson
import           API.Requests
import qualified API.Types.Chat     as C
import qualified API.Types.Message  as M
import qualified API.Types.Response as R
import qualified API.Types.Update   as U
import qualified API.Types.Inline   as I
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
  _ <- forkIO $ do
    _ <- replyToInlineQuery $ fromJust qry
    return ()
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

replyToInlineQuery :: I.InlineQuery -> IO ()
replyToInlineQuery (I.InlineQuery qId _ q _) = do 
    _ <- answerInlineQuery qId [I.InlineQueryResult "photo"
                               "derp"
                               "https://pp.vk.me/c4579/u940182/-6/x_394a5ca8.jpg"
                               "https://pp.vk.me/c4579/u940182/-6/x_394a5ca8.jpg"]
    return ()

test :: IO ()
test = print q
  where q = toJSON (I.InlineQueryResult "photo" "derp" "https://pp.vk.me/c4579/u940182/-6/x_394a5ca8.jpg" "https://pp.vk.me/c4579/u940182/-6/x_394a5ca8.jpg")
