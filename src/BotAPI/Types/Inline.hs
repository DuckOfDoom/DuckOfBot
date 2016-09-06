{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.Inline where

import           BotAPI.Types.User
import           Data.Aeson
import           Data.Monoid
import           GHC.Generics

-- InlineQuery object, as it comes from server
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


-- ChosenInlineResult object as it comes from server
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

-- A single InlineQueryResult to be sent to server
data InlineQueryResult = InlineQueryResult
                            { resultId       :: String
                            , resultType     :: String
                            , resultPhotoURL :: Maybe String
                            , resultGifURL   :: Maybe String
                            , resultThumbURL :: String
                            }
                            deriving (Show, Generic)


instance ToJSON InlineQueryResult where
  toJSON = genericToJSON defaultOptions
  toEncoding (InlineQueryResult rType rId (Just rPhotoUrl) _ rThumbUrl) =
    pairs ("type" .= rType <> "id" .= rId <> "photo_url" .= rPhotoUrl <> "thumb_url" .= rThumbUrl)
  toEncoding (InlineQueryResult rType rId _ (Just rGIFUrl) rThumbUrl) =
    pairs ("type" .= rType <> "id" .= rId <> "gif_url" .= rGIFUrl <> "thumb_url" .= rThumbUrl)
  toEncoding _ = pairs mempty
