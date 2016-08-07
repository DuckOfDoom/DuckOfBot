{-# OPTIONS_GHC -Wall #-}

module Modules.Default where

import           API.Requests
import           API.Types.Message (Message, getMessageChatID, text)
import           Data.Maybe (fromMaybe)

respondToUnknown :: Message -> IO ()
respondToUnknown msg = sendMessage (getMessageChatID msg) ("Неизвестная команда: " ++ cmd)
  where cmd = head $ words $ fromMaybe "" (text msg)

respondToPi :: Message -> IO ()
respondToPi msg = sendPhoto (getMessageChatID msg) "pi.png"
