{-# OPTIONS_GHC -Wall #-}

module Hearthstone.Util 
  ( getSearchUrl
  , readToken
  , getQueryLocale
  ) where

import           System.Environment
import           Data.Char             (toLower)

readToken :: IO String
readToken = do
  mToken <- lookupEnv "HEARTHSTONE_TOKEN"
  maybe (putStrLn "Please assign Hearthstone API token to 'HEARTHSTONE_TOKEN' environment variable!" >> return "BOT_TOKEN") return mToken

-- We need to find out what card language user is trying to get.
-- I didnt figure out a better way yet
getQueryLocale :: String -> String
getQueryLocale s | ch `elem` ['a'..'z'] = "enGB"
                 | ch `elem` ['а'..'я'] = "ruRU"
                 | otherwise = "enGB"
  where ch = toLower . head . dropWhile (== ' ') $ s

baseUrl :: String
baseUrl = "https://omgvamp-hearthstone-v1.p.mashape.com/cards"

-- API endpoint includes card name as part of adress, not a param
getSearchUrl :: String -> String
getSearchUrl name = baseUrl ++ "/search/" ++ name
