{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           API.Args
import           API.Types
import           Control.Lens  ((.~), (^.))
import           Data.Aeson    (FromJSON, eitherDecode, encode)
import           Data.Function ((&))
import           Network.Wreq  (defaults, get, getWith, param, header, post, postWith,
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

getUpdatesWithId :: Integer -> IO (Either String (Response [Update]))
getUpdatesWithId id = do
  url <- Urls.getUpdatesUrl
  response <- getWith options url 
  return (eitherDecode (response ^. responseBody) :: Either String (Response [Update]))
  where options = defaults & param "update_id" .~ ["1"]

sendMessage :: String -> String -> IO (Either String (Response Message))
sendMessage chatId messageText = do
  url <- Urls.sendMessageUrl
  response <- postWith options url (encode (SendMessageArgs chatId messageText))
  return (eitherDecode (response ^. responseBody) :: Either String (Response Message))
  where options = defaults & header "Content-Type" .~ ["application/json"]
