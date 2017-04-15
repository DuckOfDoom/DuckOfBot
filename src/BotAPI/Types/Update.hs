{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module BotAPI.Types.Update where

import           BotAPI.Types.Inline
import           BotAPI.Types.Message
import           Data.Aeson
import Control.Lens

data Update = Update
            { _updateId    :: Integer
            , _message     :: Maybe Message
            , _inlineQuery :: Maybe InlineQuery
            }
            deriving Show

makeLenses ''Update

instance FromJSON Update where
  parseJSON (Object v) = Update <$>
                         v .: "update_id" <*>
                         v .:? "message" <*>
                         v .:? "inline_query"

  parseJSON _ = fail "Failed to parse Update object!"

