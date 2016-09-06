{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Types.Response where

import           Data.Aeson

data Response a = Response
                { ok          :: Bool
                , result      :: Maybe a
                , description :: Maybe String
                , errorCode   :: Maybe Integer
                }
                deriving Show

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) = Response <$>
                         v .: "ok" <*>
                         v .:? "result" <*>
                         v .:? "description" <*>
                         v .:? "error_code"
  parseJSON _ = fail "Failed to parse Response object!"
