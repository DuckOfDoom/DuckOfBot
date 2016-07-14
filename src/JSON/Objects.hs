{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSON.Objects where

import Data.Aeson
import GHC.Generics
 
-- TODO: Figure out how to make response type polymorphic
data Response = Response 
                { ok :: Bool 
                , result :: Maybe Object 
                }
                deriving (Generic, Show)

instance FromJSON Response 

data User = User
          { id        :: Integer
          , firstName :: String
          , lastName  :: Maybe String
          , userName  :: Maybe String
          }
          deriving Show

instance FromJSON User where
  parseJSON (Object v) = User <$> 
                         v .: "id" <*> 
                         v .: "first_name" <*> 
                         v .:? "last_name" <*> 
                         v .:? "username"

  parseJSON _ = error "can't parse"

