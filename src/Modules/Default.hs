{-# OPTIONS_GHC -Wall #-}

module Modules.Default where

import           API.Requests

import           API.Types.Chat    (chatId)
import           API.Types.Message (Message, chat, text)

import           Data.Maybe

respondToUnknown :: Message -> IO ()
respondToUnknown msg = sendMessage (getChatId msg) ("Неизвестная команда: " ++ cmd)
  where cmd = head $ words $ fromMaybe "Nothing =(" (text msg)

respondToPi :: Message -> IO ()
respondToPi msg = sendPhoto (getChatId msg) "pi.png"

getChatId :: Message -> Integer
getChatId = chatId . chat
