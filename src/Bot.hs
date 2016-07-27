{-# OPTIONS_GHC -Wall #-}

module Bot where

import           Data.Aeson
import           Data.List
import           API.Requests
import qualified API.Types.Message  as M
import qualified API.Types.Response as R
import qualified API.Types.Update   as U
import qualified API.Types.Inline   as I
import           Control.Concurrent (forkIO, threadDelay)
import           Data.Maybe
import qualified Modules.Roll as Roll
import qualified Modules.Default as Default

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

-- process a single update in another thread
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

-- here is where text commands are routed
replyToMessage :: M.Message -> IO ()
replyToMessage msg | isCommand "/число" = Roll.respondToRoll msg
                   | isCommand "/пиздос" = Default.respondToPi msg
                   | otherwise = Default.respondToUnknown msg
  where isCommand = ( `isPrefixOf` fromMaybe "" (M.text msg))

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
