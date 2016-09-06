{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.Message where

import           BotAPI.Types.Chat
import           BotAPI.Types.User
import           Data.Aeson

data Message = Message
             { messageId       :: Integer
             , from            :: Maybe User
             , date            :: Integer
             , chat            :: Chat
             , forwardFrom     :: Maybe User
             , forwardFromChat :: Maybe Chat
             , forwardDate     :: Maybe Integer
             , replyToMessage  :: Maybe Message
             , editDate        :: Maybe Integer
             , text            :: Maybe String
             }
             deriving Show

instance FromJSON Message where
  parseJSON (Object v) = Message <$>
                         v .:  "message_id" <*>
                         v .:? "from" <*>
                         v .:  "date" <*>
                         v .:  "chat" <*>
                         v .:? "forward_from" <*>
                         v .:? "forward_from_chat" <*>
                         v .:? "forward_date" <*>
                         v .:? "reply_to_message" <*>
                         v .:? "edit_date" <*>
                         v .:? "text"

  parseJSON _ = fail "Failed to parse Message object!"

getMessageChatID :: Message -> Integer
getMessageChatID = chatId . chat
