{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Message where

import           BotAPI.Types.Chat
import           BotAPI.Types.User
import           Data.Aeson
import Control.Lens 

data Message = Message
             { _messageId       :: Integer
             , _from            :: Maybe User
             , _date            :: Integer
             , _chat            :: Chat
             , _forwardFrom     :: Maybe User
             , _forwardFromChat :: Maybe Chat
             , _forwardDate     :: Maybe Integer
             , _replyToMessage  :: Maybe Message
             , _editDate        :: Maybe Integer
             , _text            :: Maybe String
             }
             deriving Show

makeLenses ''Message

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
