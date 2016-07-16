{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import           JSON.Objects
import           API.Requests

--jsonTest :: IO ()
--jsonTest = print (eitherDecode "{\"ok\":true,\"result\":{\"id\":243388629,\"first_name\":\"DuckOfBot\",\"username\":\"DuckOfBot\"}}" :: Either String (Response User))

main :: IO ()
main = do
  putStrLn "Hello there! Let me check something..."
  me <- getMe
  putStrLn $ printMe me
    where printMe (Left e) = "Whoops! Something went wrong: '" ++ e ++ "'"
          printMe (Right (Response _ user)) = "All is good! Nice to meet you! My name is: " ++ firstName user
