{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module HearthstoneAPI.Requests
  ( searchCards
  ) where

import           HearthstoneAPI.Types (Card)
import           HearthstoneAPI.Util  (getQueryLocale, getSearchUrl, readToken)

import           Control.Lens         ((.~), (^.))
import           Control.Monad        (liftM)
import           Data.Function        ((&))

import           Data.Aeson           (eitherDecode)
import           Data.String          (fromString)

import           Control.Exception    (SomeException, try)
import           Data.ByteString.Lazy (ByteString)
import           Network.Wreq         (Options, Response, defaults, getWith, header,
                                       param, responseBody)

searchCards :: String -> IO [Card]
searchCards cName = do
  options <- liftM addPrms getDefaultsWithHeader
  response <- try (getWith options (getSearchUrl cName)) :: IO (Either SomeException (Response ByteString)) -- TODO: Catch only StatusCodeExceptions!
  case response of
       Left ex -> putStrLn ("[HearthstoneAPI] Caught exeption: " ++ show ex) >> return []
       Right r -> return $ toList (eitherDecode (r ^. responseBody) :: Either String [Card])
    where addPrms o = o & param "locale" .~ [fromString $ getQueryLocale cName]
          toList (Left _) = []
          toList (Right cs) = cs

getDefaultsWithHeader :: IO Options
getDefaultsWithHeader = do
  token <- readToken
  return $ defaults & header "X-Mashape-Key" .~ [fromString token]
