{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           Control.Lens ((^.))
import           Data.Aeson   (eitherDecode, FromJSON)
import           JSON.Objects
import           Network.Wreq (get, responseBody)
import qualified Util.URL      as U

makeRequest :: (FromJSON a) => String -> IO (Either String (Response a))
makeRequest url = do
  response <- get url
  return (eitherDecode (response ^. responseBody) :: (FromJSON a) => Either String (Response a))

getMe :: IO (Either String (Response User))
getMe = U.getMeUrl >>= makeRequest 

