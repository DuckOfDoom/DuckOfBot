{-# OPTIONS_GHC -Wall #-}

module UrlUtil where

import           Data.Maybe
import           System.Environment

checkToken :: IO Bool
checkToken = isJust <$> lookupEnv "BOT_TOKEN"

readToken :: IO String
readToken = do
              mToken <- lookupEnv "BOT_TOKEN"
              return $ tokenize mToken
  where tokenize (Just t) = t
        tokenize Nothing = "TOKEN_NOT_FOUND"

composeUrl :: String -> IO String
composeUrl url = do
                  t <- readToken
                  return ("https://api.telegram.org/bot" ++ t ++ "/" ++ url)

getMeUrl :: IO String
getMeUrl = composeUrl "getMe"
