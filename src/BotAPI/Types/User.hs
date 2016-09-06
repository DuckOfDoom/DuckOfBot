{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.User where

import           Data.Aeson

data User = User
          { userId    :: Integer
          , firstName :: String
          , lastName  :: Maybe String
          , username  :: Maybe String
          }
          deriving Show

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "id" <*>
                         v .: "first_name" <*>
                         v .:? "last_name" <*>
                         v .:? "username"

  parseJSON _ = fail "Failed to parse User object!"

