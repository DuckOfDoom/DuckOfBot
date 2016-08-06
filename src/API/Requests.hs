{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           API.Types
import           Control.Lens         ((.~), (^.))
import           Data.Aeson           (eitherDecode, encode)
import           Data.Function        ((&))
import           Data.String          (fromString)
import           Network.Wreq         (defaults, getWith, header, param,
                                       partFile, postWith, responseBody)
import qualified Util.URL             as Urls

-- We need these to convert lazy Bytestrings from Aeson into Text for Wreq
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Text.Internal   (Text)

-- Converts lazy bytestring that Aeson produces into a text that we can use with Wreq lenses
toText :: ByteString -> Text
toText = decodeUtf8 . toStrict

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

-- send arbitrary message to chat with specific ID
sendMessage :: Integer -> String -> IO ()
sendMessage targetChatId messageText = do
  url <- Urls.sendMessageUrl
  _ <- getWith options url
  return ()
  where options = defaults & param "chat_id" .~ [fromString $ show targetChatId]
                           & param "text"    .~ [fromString messageText]

-- send photo with a specific filename
sendPhoto :: Integer -> String -> IO ()
sendPhoto targetChatId filePath = do
  url <- Urls.sendPhotoUrl
  _ <- postWith options url (partFile "photo" filePath)
  return ()
  where options = defaults & header "Content-Type" .~ ["multipart/form-data"]
                           & param "chat_id"       .~ [fromString $ show targetChatId]

-- answer Inline Query with a specific ID and results
answerInlineQuery :: String -> [InlineQueryResult] -> IO ()
answerInlineQuery inlineQueryId results = do
  url <- Urls.answerInlineQueryUrl
  _ <- getWith options url
  return ()
  where json = encode results
        options = defaults & param "inline_query_id" .~ [fromString inlineQueryId]
                           & param "results"         .~ [toText json]
