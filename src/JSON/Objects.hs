{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}

module JSON.Objects where

import Data.Aeson
-- import GHC.Generics
 
-- Polymorphic response type for all responses 
data Response a = Response 
                { ok :: Bool 
                , result :: a 
                }
                deriving Show

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) = Response <$>
                         v .: "ok" <*>
                         v .: "result"
  parseJSON _ = fail "Failed to parse Response object!"

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

