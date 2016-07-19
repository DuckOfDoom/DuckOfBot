{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           API.Args
import           API.Types
import           Control.Lens  ((.~), (^.))
import           Data.Aeson    (FromJSON, eitherDecode, encode)
import           Data.Function ((&))
import           Data.String (fromString)
import           Network.Wreq as Wreq  (getWith, postWith, defaults, param, header, responseBody)
import           Network.Wreq.Types (Postable)

import qualified Util.URL      as Urls

-- Get something without parameters
get :: (FromJSON a) => String -> IO (Either String (Response a))
get url = do
  response <- Wreq.getWith defaults url
  return (eitherDecode (response ^. Wreq.responseBody) :: (FromJSON a) => Either String (Response a))

post :: (Postable a, FromJSON b) => String -> a -> IO (Either String (Response b))
post url payload = do
  response <- Wreq.postWith options url payload
  return (eitherDecode (response ^. Wreq.responseBody) :: (FromJSON b) => Either String (Response b))
  where options = defaults & header "Content-Type" .~ ["application/json"]

-- Get info about yourself
getMe :: IO (Either String (Response User))
getMe = Urls.getMeUrl >>= get

-- Get updates from server
getUpdates :: IO (Either String (Response [Update]))
getUpdates = Urls.getUpdatesUrl >>= get

getUpdatesWithId :: Integer -> IO (Either String (Response [Update]))
getUpdatesWithId offset = do
  url <- Urls.getUpdatesUrl
  response <- Wreq.getWith options url 
  return (eitherDecode (response ^. responseBody) :: Either String (Response [Update]))
  where options = defaults & param "offset" .~ [fromString $ show offset]

sendMessage :: Integer -> String -> IO ()
sendMessage targetChatId messageText = do 
  url <- Urls.sendMessageUrl
  _ <- post url (encode (SendMessageArgs (show targetChatId) messageText)) :: IO (Either String (Response Message))
  return ()
