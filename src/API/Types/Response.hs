{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Response where

import           Data.Aeson

-- Polymorphic response type for all responses
data Response a = Response
                { ok     :: Bool
                , result :: a
                }
                deriving Show

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) = Response <$>
                         v .: "ok" <*>
                         v .: "result"
  parseJSON _ = fail "Failed to parse Response object!"
