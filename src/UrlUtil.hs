{-# OPTIONS_GHC -Wall #-}

module UrlUtil where

-- TODO: Read token from env
token :: String
token = "nil"

baseURL :: String
baseURL = "https://api.telegram.org"

composeUrl :: String -> String
composeUrl url = baseURL ++ "/bot" ++ token ++ "/" ++ url

getMeUrl :: String 
getMeUrl = composeUrl "getMe"
