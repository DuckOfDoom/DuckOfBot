{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Inline where

import           API.Types.User  
import           Data.Aeson

-- Chat object
data InlineQuery = InlineQuery
                 { queryId     :: String
                 , queryFrom   :: User
                 , query       ::  String
                 , queryOffset :: String
                 }
                 deriving Show

instance FromJSON InlineQuery where
  parseJSON (Object v) = InlineQuery <$>
                         v .: "id" <*>
                         v .: "from" <*>
                         v .: "query" <*>
                         v .: "offset"

  parseJSON _ = fail "Failed to parse InlineQuery object!"

