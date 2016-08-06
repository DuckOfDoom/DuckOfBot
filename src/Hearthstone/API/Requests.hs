{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Hearthstone.API.Requests where

import           Hearthstone.API.Types
import           Hearthstone.Util      (getSearchUrl, readToken, getQueryLocale)

import           Control.Lens          ((.~), (^.))
import           Control.Monad         (liftM)
import           Data.Function         ((&))

import           Data.Aeson            (eitherDecode)
import           Data.String           (fromString)

import           Network.Wreq          (defaults, getWith, header, param, responseBody)


-- TODO: Catch status code exceptions!
searchCards :: String -> IO (Either String [Card])
searchCards cName = do
  options <- liftM addPrms getDefaultsWithHeader
  response <- getWith options (getSearchUrl cName)
  return (eitherDecode (response ^. responseBody) :: Either String [Card])
    where addPrms o = o & param "locale" .~ [fromString $ getQueryLocale cName]

getDefaultsWithHeader = do
  token <- readToken
  return $ defaults & header "X-Mashape-Key" .~ [fromString token]
