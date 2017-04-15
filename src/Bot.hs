module Bot
( start )
  where

import qualified BotAPI.Processing     as Processing
import qualified BotAPI.Types.Inline   as I
import qualified BotAPI.Types.Message  as M
import qualified BotAPI.Types.Update   as U

import           Control.Concurrent    (forkIO)
import           Data.List             (isPrefixOf)

import qualified Modules.Default       as Default
import qualified Modules.Hearthstone   as Hearthstone
import qualified Modules.Roll          as Roll

start :: IO ()
start = Processing.start 0.1 processUpdate

-- process a single update in another thread
processUpdate :: U.Update -> IO ()
processUpdate (U.Update _ (Just msg) Nothing) = do -- Process an Update that has a Message
  putStrLn $ "Received Message: " ++ show msg
  _ <- forkIO $ do
    _ <- replyToMessage msg
    return ()
  return ()
processUpdate (U.Update _ Nothing (Just qry)) = do  -- Process an Update that has an Inline Query
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
                   | otherwise = Default.respondToUnknown msg
  where isCommand = (`isPrefixOf` text)
replyToMessage _ = return ()

-- here is where inline queries are routed
replyToInlineQuery :: I.InlineQuery -> IO ()
replyToInlineQuery (I.InlineQuery qId _ qText _) = Hearthstone.respondToCardSearchQuery qId qText
