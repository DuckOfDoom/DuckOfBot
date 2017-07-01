module BotAPI.Requests
  ( getMe
  , getUpdates
  , getUpdatesWithId
  , sendMessage
  , sendPhoto
  , answerInlineQuery
  ) where


import BotAPI.Types (InlineQueryResult(..), Response(..), Update(..), User(..))

import           Control.Arrow      (left)
import           Control.Exception  (SomeException, try)
import           Control.Lens       ((&), (.~), (^.))
import           Data.Aeson         (eitherDecode, encode)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T (pack, unpack)
import           Network.Wreq       (Options, defaults, getWith, header, param,
                                     partFile, postWith, responseBody)
import qualified Network.Wreq       as W (Response)
import           Network.Wreq.Types (Postable)

import qualified BotAPI.URLUtil as Urls (answerInlineQueryUrl, meUrl,
                                         messageUrl, photoUrl, updatesUrl)

-- We need these to convert lazy Bytestrings from Aeson into Text for Wreq
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text.Encoding   (decodeUtf8)

-- Functions to catch exceptions in GET and POST methods.
-- To narrow down exception types we need to import a module with HttpException and that would add one more dependency =(
tryGetWith :: Options -> Text -> IO (Either Text (W.Response ByteString))
tryGetWith options url = do
  response <- try (getWith options (T.unpack url)) :: IO (Either SomeException (W.Response ByteString))
  case response of
       Left ex -> return $ Left (T.pack $ show ex)
       Right r -> return $ Right r

tryPostWith :: Postable a => Options -> Text -> a -> IO (Either Text (W.Response ByteString))
tryPostWith options url postable = do
  response <- try (postWith options (T.unpack url) postable) :: IO (Either SomeException (W.Response ByteString))
  case response of
       Left ex -> return $ Left (T.pack $ show ex)
       Right r -> return $ Right r

-- Exported functions ---------------------------

-- Get info about yourself
getMe :: IO (Either Text (Response User))
getMe = do
  url <- Urls.meUrl
  response <- tryGetWith defaults url
  case response of
    Left ex -> do
      print ("[BotAPI] Caught exception while trying to 'getMe': " <> (T.pack $ show ex))
      return (Left (T.pack $ show ex))

    Right r -> return $ (left T.pack $ eitherDecode (r ^. responseBody))

-- Get updates from server
getUpdates :: IO (Either Text (Response [Update]))
getUpdates = do
  url <- Urls.updatesUrl
  response <- tryGetWith defaults url
  case response of
       Left ex -> do
         print ("[BotAPI] Caught exception while trying to 'getUpdates': " <> (T.pack $ show ex))
         return (Left (T.pack $ show ex))
       Right r -> return (left T.pack $ eitherDecode (r ^. responseBody))

getUpdatesWithId :: Integer -> IO (Either Text (Response [Update]))
getUpdatesWithId offset = do
  url <- Urls.updatesUrl
  response <- tryGetWith options url
  case response of
       Left ex -> do 
         print ("[BotAPI] Caught exception while trying to 'getUpdatesWithId': " <> (T.pack $ show ex))
         return (Left (T.pack $ show ex))
       Right r -> return (left T.pack $ eitherDecode (r ^. responseBody) :: Either Text (Response [Update]))
  where
    options = defaults & param "offset" .~ [T.pack $ show offset]

-- send arbitrary message to chat with specific ID
sendMessage :: Integer -> Text -> IO ()
sendMessage targetChatId messageText = do
  url <- Urls.messageUrl
  response <- tryGetWith options url
  case response of
       Left ex -> do 
         print ("[BotAPI] Caught exception while trying to 'sendMessage': " <> (T.pack $ show ex))
         return ()
       Right _ -> return ()
  where
    options = defaults & param "chat_id" .~ [T.pack $ show targetChatId]
                       & param "text"    .~ [messageText]

-- send photo with a specific filename
sendPhoto :: Integer -> Text -> IO ()
sendPhoto targetChatId filePath = do
  url <- Urls.photoUrl
  response <- tryPostWith options url (partFile "photo" $ T.unpack filePath)
  case response of
       Left ex -> do
         print ("[BotAPI] Caught exception while trying to 'sendPhoto': " <> (T.pack $ show ex))
         return ()
       Right _ -> return ()
  where options = defaults & header "Content-Type" .~ ["multipart/form-data"]
                           & param "chat_id"       .~ [T.pack $ show targetChatId]

-- answer Inline Query with a specific ID and results
answerInlineQuery :: Text -> [InlineQueryResult] -> IO ()
answerInlineQuery inlineQueryId results = do
  url <- Urls.answerInlineQueryUrl
  response <- tryGetWith options url
  case response of
       Left ex -> do
         print ("[BotAPI] Caught exception while trying to 'answerInlineQuery': " <> (T.pack $  show ex))
         return ()
       Right _ -> return ()
  where
    json = (decodeUtf8 . toStrict . encode) results
    options = defaults & param "inline_query_id" .~ [inlineQueryId]
                       & param "results"         .~ [json]
