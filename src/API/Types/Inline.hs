{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Inline where

import           API.Types.User  
import           Data.Aeson

-- Chat object
data InlineQuery = InlineQuery
                 { queryId     :: String
                 , queryFrom   :: User
                 , query       :: String
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


data ChosenInlineResult = ChosenInlineResult
                        { chosenResultId    :: String
                        , chosenResultFrom  :: User
                        , chosenResultQuery :: String
                        }

instance FromJSON ChosenInlineResult where
  parseJSON (Object v) = ChosenInlineResult <$>
                         v .: "result_id" <*>
                         v .: "from" <*>
                         v .: "query"

  parseJSON _ = fail "Failed to parse ChosenInlineResult object!"

-- A single type for all query results. 
-- Since API decides by the "type" field, which result it is, we can use one type for all results
data InlineQueryResult = InlineQueryResult
                            { resultType :: String
                            , resultId   :: String
                            , resultPhotoURL :: String
                            , resultThumbURL :: String
                            }


instance ToJSON InlineQueryResult where
  toJSON (InlineQueryResult rType rId rPhotoUrl rThumbUrl) = 
    object [ "type" .= rType
           , "id" .= rId
           , "photo_url" .= rPhotoUrl
           , "thumb_url" .= rThumbUrl
           ]
