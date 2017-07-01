{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Message
  ( Message(..)
  , messageId
  , from
  , date
  , chat
  , forwardFrom
  , forwardFromChat
  , forwardDate
  , replyToMessage
  , editDate
  , text
  )
  where

import BotAPI.Types.Chat (Chat(..))
import BotAPI.Types.User (User(..))
import Control.Lens      (makeLenses)
import Data.Aeson        (FromJSON(..), Value(..), parseJSON, (.:), (.:?))
import Data.Text         (Text)

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
  , _text            :: Maybe Text
  }
  deriving Show

makeLenses ''Message

instance FromJSON Message where
  parseJSON (Object v) =
    Message                   <$>
    v .:  "message_id"        <*>
    v .:? "from"              <*>
    v .:  "date"              <*>
    v .:  "chat"              <*>
    v .:? "forward_from"      <*>
    v .:? "forward_from_chat" <*>
    v .:? "forward_date"      <*>
    v .:? "reply_to_message"  <*>
    v .:? "edit_date"         <*>
    v .:? "text"

  parseJSON _ = fail "Failed to parse Message object!"
