{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           API.Args
import           API.Types
import           Control.Lens  ((.~), (^.))
import           Data.Aeson    (FromJSON, eitherDecode, encode)
import           Data.Function ((&))
import           Network.Wreq  (defaults, get, header, post, postWith,
                                responseBody)

import qualified Util.URL      as Urls

-- TODO: Make get and post requests with polymorphism for different responses

makeRequest :: (FromJSON a) => String -> IO (Either String (Response a))
makeRequest url = do
  response <- get url
  return (eitherDecode (response ^. responseBody) :: (FromJSON a) => Either String (Response a))

-- Get info about yourself
getMe :: IO (Either String (Response User))
getMe = Urls.getMeUrl >>= makeRequest

-- Get updates from server
getUpdates :: IO (Either String (Response [Update]))
getUpdates = Urls.getUpdatesUrl >>= makeRequest

sendMessage :: SendMessageArgs -> IO (Either String (Response Message))
sendMessage args = do
  url <- Urls.sendMessageUrl
  response <- postWith options url (encode args)
  return (eitherDecode (response ^. responseBody) :: Either String (Response Message))
  where options = defaults & header "Content-Type" .~ ["application/json"]
