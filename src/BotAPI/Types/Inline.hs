{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}

module BotAPI.Types.Inline where

import           BotAPI.Types.User
import           Data.Aeson
import           Data.Monoid
import           GHC.Generics
import Control.Lens (makeLenses)


-- InlineQuery object, as it comes from server
data InlineQuery = InlineQuery
                 { _queryId     :: String
                 , _queryFrom   :: User
                 , _query       :: String
                 , _queryOffset :: String
                 }
                 deriving Show

makeLenses ''InlineQuery

instance FromJSON InlineQuery where
  parseJSON (Object v) = InlineQuery <$>
                         v .: "id" <*>
                         v .: "from" <*>
                         v .: "query" <*>
                         v .: "offset"

  parseJSON _ = fail "Failed to parse InlineQuery object!"


-- ChosenInlineResult object as it comes from server
data ChosenInlineResult = ChosenInlineResult
                        { _chosenResultId    :: String
                        , _chosenResultFrom  :: User
                        , _chosenResultQuery :: String
                        }

makeLenses ''ChosenInlineResult

instance FromJSON ChosenInlineResult where
  parseJSON (Object v) = ChosenInlineResult <$>
                         v .: "result_id" <*>
                         v .: "from" <*>
                         v .: "query"

  parseJSON _ = fail "Failed to parse ChosenInlineResult object!"

-- A single InlineQueryResult to be sent to server
data InlineQueryResult = InlineQueryResult
                            { _resultId       :: String
                            , _resultType     :: String
                            , _resultPhotoURL :: Maybe String
                            , _resultGifURL   :: Maybe String
                            , _resultThumbURL :: String
                            }
                            deriving (Show, Generic)

makeLenses ''InlineQueryResult

instance ToJSON InlineQueryResult where
  toJSON = genericToJSON defaultOptions
  toEncoding (InlineQueryResult rType rId (Just rPhotoUrl) _ rThumbUrl) =
    pairs ("type" .= rType <> "id" .= rId <> "photo_url" .= rPhotoUrl <> "thumb_url" .= rThumbUrl)
  toEncoding (InlineQueryResult rType rId _ (Just rGIFUrl) rThumbUrl) =
    pairs ("type" .= rType <> "id" .= rId <> "gif_url" .= rGIFUrl <> "thumb_url" .= rThumbUrl)
  toEncoding _ = pairs mempty
