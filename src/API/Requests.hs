{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           Control.Lens ((^.))
import           Data.Aeson   (FromJSON, eitherDecode)
import           API.Types
import           Network.Wreq (get, responseBody)

import qualified Util.URL  as Urls

-- Polymorphic function to make request to some URL and get some response
makeRequest :: (FromJSON a) => String -> IO (Either String (Response a))
makeRequest url = do
  response <- get url
  return (eitherDecode (response ^. responseBody) :: (FromJSON a) => Either String (Response a))

--makeRequestWith :: (FromJSON a) => String -> Options -> IO (Either String (Response a))
--makeRequestWith url options = do
--  response <- getWith options url
--  return (eitherDecode (response ^. responseBody) :: (FromJSON a) => Either String (Response a))

-- Get info about yourself
getMe :: IO (Either String (Response User))
getMe = Urls.getMeUrl >>= makeRequest

-- Get updates from server
getUpdates :: IO (Either String (Response [Update]))
getUpdates = Urls.getUpdatesUrl >>= makeRequest

-- Get upates from server with specific offset
--getUpdates :: Integer -> IO (Either String (Response [Update]))
--getUpdates = do
--  url <- Urls.getUpdatesUrl
--  opts <- defaults >>= makeRequest
