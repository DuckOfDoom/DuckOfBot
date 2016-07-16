{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Update where

import           API.Types.Message
import           Data.Aeson

data Update = Update
            { updateID      :: Integer
            , message       :: Maybe Message
            , editedMessage :: Maybe Message
            }
            deriving Show

instance FromJSON Update where
  parseJSON (Object v) = Update <$>
                         v .: "update_id" <*>
                         v .:? "message" <*>
                         v .:? "edited_message"

  parseJSON _ = fail "Failed to parse Update object!"

