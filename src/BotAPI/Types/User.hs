{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.User
  ( User(..)
  , userId
  , firstName
  , lastName
  , username
  )
  where

import Control.Lens (makeLenses)
import Data.Aeson   (FromJSON(..), Value(..), parseJSON, (.:), (.:?))
import Data.Text    (Text)

data User = User
  { _userId    :: Integer
  , _firstName :: Text
  , _lastName  :: Maybe Text
  , _username  :: Maybe Text
  }
  deriving Show

makeLenses ''User

instance FromJSON User where
  parseJSON (Object v) =
    User               <$>
    v .:  "id"         <*>
    v .:  "first_name" <*>
    v .:? "last_name"  <*>
    v .:? "username"

  parseJSON _ = fail "Failed to parse User object!"

