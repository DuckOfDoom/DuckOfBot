module Modules.Default where

import           BotAPI.Requests
import           BotAPI.Types.Message 
import           BotAPI.Types.Chat
import           Data.Maybe (fromMaybe)
import           Control.Lens

respondToUnknown :: Message -> IO ()
respondToUnknown msg = sendMessage (msg ^. (chat . chatId)) ("Неизвестная команда: " ++ cmd)
  where cmd = head $ words $ fromMaybe "" (msg ^. text)

respondToPi :: Message -> IO ()
respondToPi msg = sendPhoto (msg ^. (chat . chatId)) "pi.png"
