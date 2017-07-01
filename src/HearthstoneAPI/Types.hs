{-# LANGUAGE DeriveGeneric #-}

module HearthstoneAPI.Types where

import Data.Aeson
import Data.Text    (Text)
import GHC.Generics

data Card = Card
  { cardId  :: Text
  , name    :: Text
  , cardSet :: Maybe Text
  , img     :: Maybe Text
  , imgGold :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Card where
