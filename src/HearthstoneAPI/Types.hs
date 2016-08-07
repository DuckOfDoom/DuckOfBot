{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HearthstoneAPI.Types where 

import           Data.Aeson
import           GHC.Generics

data Card = Card
          { cardId  :: String
          , name    :: String
          , cardSet :: Maybe String
          , img     :: Maybe String
          , imgGold :: Maybe String
          }
          deriving (Show, Generic)

instance FromJSON Card where
