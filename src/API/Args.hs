{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Args where

import           Data.Aeson   (ToJSON, toJSON, object, (.=))

data SendMessageArgs = SendMessageArgs
                     { messageChatId :: String
                     , text    :: String
                     }
                     deriving Show

instance ToJSON SendMessageArgs where 
  toJSON (SendMessageArgs sendMessageChatId txt) = 
    object ["chat_id" .= sendMessageChatId, "text" .= txt]
