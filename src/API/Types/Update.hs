{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Update where

import           API.Types.Message
import           API.Types.Inline
import           Data.Aeson

data Update = Update
            { updateId      :: Integer
            , message       :: Maybe Message
            , inlineQuery   :: Maybe InlineQuery
            }
            deriving Show

instance FromJSON Update where
  parseJSON (Object v) = Update <$>
                         v .: "update_id" <*>
                         v .:? "message" <*>
                         v .:? "inline_query"

  parseJSON _ = fail "Failed to parse Update object!"

