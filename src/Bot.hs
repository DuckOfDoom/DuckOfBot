{-# OPTIONS_GHC -Wall #-}

module Bot 
  ( startGetUpdatesLoopWithDelay 
  ) where

import           API.Requests
import qualified API.Types.Inline    as I
import qualified API.Types.Message   as M
import qualified API.Types.Response  as R
import qualified API.Types.Update    as U
import           Control.Concurrent  (forkIO, threadDelay)
import           Data.List

import qualified Modules.Countdown   as Countdown
import qualified Modules.Default     as Default
import qualified Modules.Hearthstone as Hearthstone
import qualified Modules.Roll        as Roll

startGetUpdatesLoopWithDelay :: Float -> IO ()
startGetUpdatesLoopWithDelay delay = do
  response <- getUpdates
  lastUpdateId <- processUpdatesAndReturnLastId response
  getUpdatesLoop delay lastUpdateId
    where getUpdatesLoop d lastId = do
            response <- getUpdatesWithId (lastId + 1)
            newId <- processUpdatesAndReturnLastId response
            threadDelay $ truncate (1000000 * delay)
            getUpdatesLoop d newId

processUpdatesAndReturnLastId :: Either String (R.Response [U.Update]) -> IO Integer
processUpdatesAndReturnLastId (Left ex) = putStrLn ("[Bot] Failed to process updates: " ++ ex) >> return 0
processUpdatesAndReturnLastId (Right (R.Response _ Nothing _ _)) = return 0
processUpdatesAndReturnLastId (Right (R.Response _ (Just []) _ _)) = return 0
processUpdatesAndReturnLastId (Right (R.Response _ (Just updates) _ _)) = do
  mapM_ processUpdate updates
  return $ (U.updateId . last) updates

-- process a single update in another thread
processUpdate :: U.Update -> IO ()
processUpdate (U.Update _ (Just msg) Nothing) = do -- Process an Update that has a Message
  putStrLn $ "Received Message: " ++ show msg
  _ <- forkIO $ do
    _ <- replyToMessage msg
    return ()
  return ()
processUpdate (U.Update _ Nothing (Just qry)) = do  -- Process and Update that has an Inline Query
  putStrLn $ "Received InlineQuery: " ++ show qry
  _ <- forkIO $ do
    _ <- replyToInlineQuery qry
    return ()
  return ()
processUpdate u = do
  putStrLn $ "Dont know how to process update: " ++ show u
  return ()

-- here is where text commands are routed
replyToMessage :: M.Message -> IO ()
replyToMessage msg@(M.Message _ _ _ _ _ _ _ _ _ (Just text))
                   | isCommand "/число" || isCommand "/roll" = Roll.respondToRoll msg
                   | isCommand "/пиздос" = Default.respondToPi msg
                   | isCommand "/когдатамлегион" = Countdown.respondToLegionCountdown msg
                   | isCommand "/когдатамкаражан" = Countdown.respondToKharazhanCountdown msg
                   | otherwise = Default.respondToUnknown msg
  where isCommand = (`isPrefixOf` text)
replyToMessage _ = return ()

-- here is where inline queries are routed
replyToInlineQuery :: I.InlineQuery -> IO ()
replyToInlineQuery (I.InlineQuery qId _ qText _) = Hearthstone.respondToCardSearchQuery qId qText
