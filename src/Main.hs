{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens  ((^.))
import           Control.Monad (unless)
import           Data.Aeson
import           JSON.Objects
import           Network.Wreq  (get, responseBody)
import qualified UrlUtil       as U

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
  unless hasToken (putStrLn "Please assign token to 'BOT_TOKEN' environment variable!")

