{-# OPTIONS_GHC -Wall #-}

module Bot where

import           API.Requests
import qualified API.Types.Inline   as I
import qualified API.Types.Message  as M
import qualified API.Types.Response as R
import qualified API.Types.Update   as U
import           Control.Concurrent (forkIO, threadDelay)
import           Data.List
import           Data.Maybe

import qualified Modules.Countdown  as Countdown
import qualified Modules.Default    as Default
import qualified Modules.Roll       as Roll

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
replyToMessage msg | isCommand "/число" || isCommand "/roll" = Roll.respondToRoll msg
                   | isCommand "/пиздос" = Default.respondToPi msg
                   | isCommand "/когдатамлегион" = Countdown.respondToLegionCountdown msg
                   | isCommand "/когдатамкаражан" = Countdown.respondToKharazhanCountdown msg
                   | otherwise = Default.respondToUnknown msg
  where isCommand = ( `isPrefixOf` fromMaybe "" (M.text msg))


replyToInlineQuery :: I.InlineQuery -> IO ()
replyToInlineQuery (I.InlineQuery qId _ _ _) = do
    _ <- answerInlineQuery qId [I.InlineQueryResult "photo"
                               "derp"
                               "https://pp.vk.me/c637330/v637330182/21c0/xJxt12gw5To.jpg"
                               "https://pp.vk.me/c637330/v637330182/21c0/xJxt12gw5To.jpg"]
    return ()
