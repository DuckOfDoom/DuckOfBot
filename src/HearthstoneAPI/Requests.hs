{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module HearthstoneAPI.Requests where

import           HearthstoneAPI.Types
import           HearthstoneAPI.Util      (getSearchUrl, readToken, getQueryLocale)

import           Control.Lens          ((.~), (^.))
import           Control.Monad         (liftM)
import           Data.Function         ((&))

import           Data.Aeson            (eitherDecode)
import           Data.String           (fromString)

import           Network.Wreq          (defaults, getWith, header, param, responseBody)


searchCards :: String -> IO [Card]
searchCards cName = do
  options <- liftM addPrms getDefaultsWithHeader
  response <- getWith options (getSearchUrl cName) -- TODO: Return empty list on 404!
  return $ toList (eitherDecode (response ^. responseBody) :: Either String [Card])
    where addPrms o = o & param "locale" .~ [fromString $ getQueryLocale cName]
          toList (Left _) = []
          toList (Right cs) = cs

-- I have no idea how to write a signature for this function. 
-- Should be IO Options, but Options is in a hidden module...
getDefaultsWithHeader = do
  token <- readToken
  return $ defaults & header "X-Mashape-Key" .~ [fromString token]
