{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^.))
import           Data.Aeson
import           JSON.Objects
import           Network.Wreq (get, responseBody)
import qualified UrlUtil      as U

testRequest :: IO ()
testRequest = do
  url <- U.getMeUrl
  r <- get url
  putStr $ show $ r ^. responseBody

jsonTest :: IO ()
jsonTest = print (decode "{\"ok\":true,\"result\":{\"id\":243388629,\"first_name\":\"DuckOfBot\",\"username\":\"DuckOfBot\"}}" :: Maybe Response)

main :: IO ()
main = do
    putStrLn "Hello there!"
    hasToken <- U.checkToken
    if (hasToken) then
      putStrLn "Nice token you got!"
    else
      putStrLn "Invalid token, bro!"
      
