{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Inline 
  ( InlineQuery(..)
  , queryId
  , queryFrom
  , query
  , queryOffset
  , ChosenInlineResult(..)
  , chosenResultId
  , chosenResultFrom
  , chosenResultQuery
  , InlineQueryResult(..)
  , resultId       
  , resultType     
  , resultPhotoURL 
  , resultGifURL  
  , resultThumbURL
  )
  where

import BotAPI.Types.User (User(..))
import Control.Lens      (makeLenses)
import Data.Monoid       ((<>))
import Data.Text         (Text)
import GHC.Generics      (Generic(..))

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), defaultOptions,
                   genericToJSON, pairs, parseJSON, (.:), (.=))

-- InlineQuery object, as it comes from server
data InlineQuery = InlineQuery
  { _queryId     :: Text
  , _queryFrom   :: User
  , _query       :: Text
  , _queryOffset :: Text
  }
  deriving Show

makeLenses ''InlineQuery

instance FromJSON InlineQuery where
  parseJSON (Object v) =
    InlineQuery  <$>
    v .: "id"    <*>
    v .: "from"  <*>
    v .: "query" <*>
    v .: "offset"

  parseJSON _ = fail "Failed to parse InlineQuery object!"


-- ChosenInlineResult object as it comes from server
data ChosenInlineResult = ChosenInlineResult
  { _chosenResultId    :: Text
  , _chosenResultFrom  :: User
  , _chosenResultQuery :: Text
  }

makeLenses ''ChosenInlineResult

instance FromJSON ChosenInlineResult where
  parseJSON (Object v) =
    ChosenInlineResult <$>
    v .: "result_id"   <*>
    v .: "from"        <*>
    v .: "query"

  parseJSON _ = fail "Failed to parse ChosenInlineResult object!"

-- A single InlineQueryResult to be sent to server
data InlineQueryResult = InlineQueryResult
  { _resultId       :: Text
  , _resultType     :: Text
  , _resultPhotoURL :: Maybe Text
  , _resultGifURL   :: Maybe Text
  , _resultThumbURL :: Text
  }
  deriving (Show, Generic)

makeLenses ''InlineQueryResult

instance ToJSON InlineQueryResult where
  toJSON = genericToJSON defaultOptions
  toEncoding (InlineQueryResult rType rId (Just rPhotoUrl) _ rThumbUrl) =
    pairs $
      "type"      .= rType     <>
      "id"        .= rId       <>
      "photo_url" .= rPhotoUrl <>
      "thumb_url" .= rThumbUrl

  toEncoding (InlineQueryResult rType rId _ (Just rGIFUrl) rThumbUrl) =
    pairs $
      "type"      .= rType   <>
      "id"        .= rId     <>
      "gif_url"   .= rGIFUrl <>
      "thumb_url" .= rThumbUrl

  toEncoding _ = pairs mempty
