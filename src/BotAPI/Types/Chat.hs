{-# LANGUAGE TemplateHaskell #-}


module BotAPI.Types.Chat where

import           Data.Aeson
import Control.Lens (makeLenses)

-- Chat object
data Chat = Chat
          { _chatId        :: Integer
          , _chatType      :: String
          , _title         :: Maybe String
          , _chatUsername  :: Maybe String
          , _chatFirstName :: Maybe String
          , _chatLastName  :: Maybe String
          }
          deriving Show

makeLenses ''Chat

instance FromJSON Chat where
  parseJSON (Object v) = Chat <$>
                         v .:  "id" <*>
                         v .:  "type" <*>
                         v .:? "title" <*>
                         v .:? "username" <*>
                         v .:? "first_name" <*>
                         v .:? "last_name"

  parseJSON _ = fail "Failed to parse Chat object!"

