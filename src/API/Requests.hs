{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           Control.Lens ((^.))
import           Data.Aeson   (FromJSON, eitherDecode)
import           API.Types
import           Network.Wreq (get, responseBody)

import qualified Util.URL  as Urls

makeRequest :: (FromJSON a) => String -> IO (Either String (Response a))
makeRequest url = do
  response <- get url
  return (eitherDecode (response ^. responseBody) :: (FromJSON a) => Either String (Response a))

getMe :: IO (Either String (Response User))
getMe = Urls.getMeUrl >>= makeRequest

getUpdates :: IO (Either String (Response [Update]))
getUpdates = Urls.getUpdatesUrl >>= makeRequest
