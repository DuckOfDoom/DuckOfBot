{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.Update where

import           BotAPI.Types.Inline
import           BotAPI.Types.Message
import           Data.Aeson

data Update = Update
            { updateId    :: Integer
            , message     :: Maybe Message
            , inlineQuery :: Maybe InlineQuery
            }
            deriving Show

instance FromJSON Update where
  parseJSON (Object v) = Update <$>
                         v .: "update_id" <*>
                         v .:? "message" <*>
                         v .:? "inline_query"

  parseJSON _ = fail "Failed to parse Update object!"

