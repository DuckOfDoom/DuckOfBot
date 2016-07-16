{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Requests where

import           Control.Lens ((^.))
import           Data.Aeson   (eitherDecode)
import           JSON.Objects
import           Network.Wreq (get, responseBody)
import qualified UrlUtil      as U

-- TODO: Figure out how to make polymorphic request function. This doesnt work =(
--makeRequest :: (FromJSON a) => String -> IO (Either String (Response a))
--makeRequest url = let maybeToEither e = maybe (Left e) Right
--                      in do
--                        response <- get url
--                        return $ maybeToEither ("Request to '" ++ url ++ "' failed") (decode (response ^. responseBody) :: Maybe (Response a))

getMe :: IO (Either String (Response User))
getMe = do
  url <- U.getMeUrl
  r <- get url
  return (eitherDecode (r ^. responseBody) :: Either String (Response User))
