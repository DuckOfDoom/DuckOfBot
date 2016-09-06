{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Requests
  ( getMe
  , getUpdates
  , getUpdatesWithId
  , sendMessage
  , sendPhoto
  , answerInlineQuery
  ) where

import           BotAPI.Types
import           Control.Exception    (SomeException, try)
import           Control.Lens         ((.~), (^.))
import           Data.Aeson           (eitherDecode, encode)
import           Data.Function        ((&))
import           Data.String          (fromString)
import           Network.Wreq         (Options, defaults, getWith, header,
                                       param, partFile, postWith, responseBody)
import           Network.Wreq.Types   (Postable)

import qualified Network.Wreq         as W (Response)
import qualified BotAPI.URLUtil as Urls

-- We need these to convert lazy Bytestrings from Aeson into Text for Wreq
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Text.Internal   (Text)

-- Utility functions -----------------------------

-- Converts lazy bytestring that Aeson produces into a text that we can use with Wreq lenses
toText :: ByteString -> Text
toText = decodeUtf8 . toStrict

-- Functions to catch exceptions in GET and POST methods.
-- To narrow down exception types we need to import a module with HttpException and that would add one more dependency =(
tryGetWith :: Options -> String -> IO (Either String (W.Response ByteString))
tryGetWith options url = do
  response <- try (getWith options url) :: IO (Either SomeException (W.Response ByteString))
  case response of
       Left ex -> return $ Left (show ex)
       Right r -> return $ Right r

tryPostWith :: Postable a => Options -> String -> a -> IO (Either String (W.Response ByteString))
tryPostWith options url postable = do
  response <- try (postWith options url postable) :: IO (Either SomeException (W.Response ByteString))
  case response of
       Left ex -> return $ Left (show ex)
       Right r -> return $ Right r

-- Exported functions ---------------------------

-- Get info about yourself
getMe :: IO (Either String (Response User))
getMe = do
  url <- Urls.getMeUrl
  response <- tryGetWith defaults url
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'getMe': " ++ show ex) >> return (Left (show ex))
       Right r -> return (eitherDecode (r ^. responseBody) :: Either String (Response User))

-- Get updates from server
getUpdates :: IO (Either String (Response [Update]))
getUpdates = do
  url <- Urls.getUpdatesUrl
  response <- tryGetWith defaults url
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'getUpdates': " ++ show ex) >> return (Left (show ex))
       Right r -> return (eitherDecode (r ^. responseBody) :: Either String (Response [Update]))

getUpdatesWithId :: Integer -> IO (Either String (Response [Update]))
getUpdatesWithId offset = do
  url <- Urls.getUpdatesUrl
  response <- tryGetWith options url
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'getUpdatesWithId': " ++ show ex) >> return (Left (show ex))
       Right r -> return (eitherDecode (r ^. responseBody) :: Either String (Response [Update]))
  where options = defaults & param "offset" .~ [fromString $ show offset]

-- send arbitrary message to chat with specific ID
sendMessage :: Integer -> String -> IO ()
sendMessage targetChatId messageText = do
  url <- Urls.sendMessageUrl
  response <- tryGetWith options url
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'sendMessage': " ++ show ex) >> return ()
       Right _ -> return ()
  where options = defaults & param "chat_id" .~ [fromString $ show targetChatId]
                           & param "text"    .~ [fromString messageText]

-- send photo with a specific filename
sendPhoto :: Integer -> String -> IO ()
sendPhoto targetChatId filePath = do
  url <- Urls.sendPhotoUrl
  response <- tryPostWith options url (partFile "photo" filePath)
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'sendPhoto': " ++ show ex) >> return ()
       Right _ -> return ()
  where options = defaults & header "Content-Type" .~ ["multipart/form-data"]
                           & param "chat_id"       .~ [fromString $ show targetChatId]

-- answer Inline Query with a specific ID and results
answerInlineQuery :: String -> [InlineQueryResult] -> IO ()
answerInlineQuery inlineQueryId results = do
  url <- Urls.answerInlineQueryUrl
  response <- tryGetWith options url
  case response of
       Left ex -> putStrLn ("[BotAPI] Caught exception while trying to 'answerInlineQuery': " ++ show ex) >> return ()
       Right _ -> return ()
  where json = encode results
        options = defaults & param "inline_query_id" .~ [fromString inlineQueryId]
                           & param "results"         .~ [toText json]
