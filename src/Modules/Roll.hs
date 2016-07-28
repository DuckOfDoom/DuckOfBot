{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Modules.Roll 
  --(respondToRoll)
  where

import           System.Random     (randomRIO)

import           API.Requests
import           API.Types.Message
import           API.Types.User
import           API.Types.Chat

import           Text.Read (readMaybe)
import           Data.Maybe 
import  Data.Char
import           Control.Arrow ((>>>))

respondToRoll :: Message -> IO ()
respondToRoll msg = do 
 rollResult <- roll args
 sendMessage (chatId $ chat msg) (getRollMessage name rollResult args)
  where name = firstName $ fromJust $ from msg
        messageText = fromMaybe "" (text msg)
        args = readArgs messageText

getRollMessage :: String -> Int -> (Int, Int) -> String 
getRollMessage name result (l, u) = name ++ " выбрасывает " ++ show result ++ " (" ++ show l ++ "-" ++ show u ++ ")"

roll :: (Int, Int) -> IO Int
roll (l, u) | u < l = roll (u, l)
            | otherwise = randomRIO (l, u)

readArgs :: String -> (Int, Int)
readArgs = takeTuple >>> replaceDash >>> readResult >>> fromMaybe (1, 100)
  where takeTuple = dropWhile (not . isDigit) >>> takeWhile (\s -> (isDigit s) || (s == '-') || (s == ' '))
        replaceDash = map (\c -> if c == '-' then ',' else c) 
        readResult x = readMaybe ("(" ++ x ++ ")") :: Maybe (Int, Int)
