{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Update
  ( Update(..)
  , updateId
  , message
  , inlineQuery
  )
  where

import BotAPI.Types.Inline  (InlineQuery(..))
import BotAPI.Types.Message (Message(..))

import Control.Lens         (makeLenses)
import Data.Aeson           (FromJSON(..), Value(..), parseJSON, (.:), (.:?))

data Update = Update
  { _updateId    :: Integer
  , _message     :: Maybe Message
  , _inlineQuery :: Maybe InlineQuery
  }
  deriving Show

makeLenses ''Update

instance FromJSON Update where
  parseJSON (Object v) =
    Update            <$>
    v .:  "update_id" <*>
    v .:? "message"   <*>
    v .:? "inline_query"

  parseJSON _ = fail "Failed to parse Update object!"

