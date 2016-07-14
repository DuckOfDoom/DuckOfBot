{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens ((^.))
import           Data.Aeson
import           JSON.Objects
import           Network.Wreq (get, responseBody)
import qualified UrlUtil      as U

testRequest :: IO ()
testRequest = do
  r <- get U.getMeUrl
  putStr $ show $ r ^. responseBody

jsonTest :: IO ()
jsonTest = print (decode "{\"ok\":true,\"result\":{\"id\":243388629,\"first_name\":\"DuckOfBot\",\"username\":\"DuckOfBot\"}}" :: Maybe Response)

main :: IO ()
main = putStrLn "DERP"
