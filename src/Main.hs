{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import           API.Requests
import           API.Types

import           Bot
import           Control.Monad (when)
import           Data.Either   (isRight)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
  putStrLn "Hello there! Let me check something..."
  me <- getMe
  putStrLn $ printMe me
  when (isRight me) (startGetUpdatesLoopWithDelay 0.1)
  where printMe (Left e) = "Whoops! Something went wrong: '" ++ e ++ "'"
        printMe (Right (Response _ user _ _ )) = "All is good! Nice to meet you! My name is: " ++ firstName (fromJust user)
