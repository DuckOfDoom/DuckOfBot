{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.User where

import           Data.Aeson
import           Control.Lens

data User = User
          { _userId    :: Integer
          , _firstName :: String
          , _lastName  :: Maybe String
          , _username  :: Maybe String
          }
          deriving Show

makeLenses ''User

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "id" <*>
                         v .: "first_name" <*>
                         v .:? "last_name" <*>
                         v .:? "username"

  parseJSON _ = fail "Failed to parse User object!"

