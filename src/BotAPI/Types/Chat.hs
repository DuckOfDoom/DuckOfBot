{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.Chat where

import           Data.Aeson

-- Chat object
data Chat = Chat
          { chatId        :: Integer
          , chatType      :: String
          , title         :: Maybe String
          , chatUsername  :: Maybe String
          , chatFirstName :: Maybe String
          , chatLastName  :: Maybe String
          }
          deriving Show

instance FromJSON Chat where
  parseJSON (Object v) = Chat <$>
                         v .:  "id" <*>
                         v .:  "type" <*>
                         v .:? "title" <*>
                         v .:? "username" <*>
                         v .:? "first_name" <*>
                         v .:? "last_name"

  parseJSON _ = fail "Failed to parse Chat object!"

