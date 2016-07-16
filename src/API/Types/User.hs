{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.User where

import           Data.Aeson 

-- User object
data User = User
          { id        :: Integer
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

