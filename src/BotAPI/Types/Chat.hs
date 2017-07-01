{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Chat
  ( Chat(..)
  , chatId
  , chatType
  , title
  , chatUsername
  , chatFirstName
  , chatLastName
  )
  where

import Control.Lens (makeLenses)
import Data.Aeson   (FromJSON(..), Value(..), parseJSON, (.:), (.:?))
import Data.Text    (Text)

-- Chat object
data Chat = Chat
  { _chatId        :: Integer
  , _chatType      :: Text
  , _title         :: Maybe Text
  , _chatUsername  :: Maybe Text
  , _chatFirstName :: Maybe Text
  , _chatLastName  :: Maybe Text
  }
  deriving Show

makeLenses ''Chat

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat               <$>
    v .:  "id"         <*>
    v .:  "type"       <*>
    v .:? "title"      <*>
    v .:? "username"   <*>
    v .:? "first_name" <*>
    v .:? "last_name"

  parseJSON _ = fail "Failed to parse Chat object!"

