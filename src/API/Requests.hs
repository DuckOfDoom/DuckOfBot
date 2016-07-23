{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           API.Types
import           Control.Lens  ((.~), (^.))
import           Data.Aeson    (eitherDecode)
import           Data.Function ((&))
import           Data.String   (fromString)
import           Network.Wreq  (defaults, getWith, header, param, partFile,
                                postWith, responseBody)
import qualified Util.URL      as Urls

-- Get info about yourself
getMe :: IO (Either String (Response User))
getMe = do
    url <- Urls.getMeUrl
    response <- getWith defaults url
    return (eitherDecode (response ^. responseBody) :: Either String (Response User))

-- Get updates from server
getUpdates :: IO (Either String (Response [Update]))
getUpdates = do
  url <- Urls.getUpdatesUrl
  response <- getWith defaults url
  return (eitherDecode (response ^. responseBody) :: Either String (Response [Update]))

getUpdatesWithId :: Integer -> IO (Either String (Response [Update]))
getUpdatesWithId offset = do
  url <- Urls.getUpdatesUrl
  response <- getWith options url
  return (eitherDecode (response ^. responseBody) :: Either String (Response [Update]))
  where options = defaults & param "offset" .~ [fromString $ show offset]

sendMessage :: Integer -> String -> IO ()
sendMessage targetChatId messageText = do
  url <- Urls.sendMessageUrl
  _ <- getWith options url 
  return ()
  where options = defaults & param "chat_id" .~ [fromString $ show targetChatId]
                           & param "text"    .~ [fromString messageText]

-- sendPhoto takes a chat ID and a filename of the photo relative to exe dir
sendPhoto :: Integer -> String -> IO ()
sendPhoto targetChatId filePath = do
  url <- Urls.sendPhotoUrl
  _ <- postWith options url (partFile "photo" filePath)
  return ()
  where options = defaults & header "Content-Type" .~ ["multipart/form-data"]
                           & param "chat_id"       .~ [fromString $ show targetChatId]

--answerInlineQuery String -> [InlineQueryResult] -> IO ()
--answerInlineQuery queryId results = do
--  url <- Urls.answerInlineQueryUrl
--  _ <- post options url
