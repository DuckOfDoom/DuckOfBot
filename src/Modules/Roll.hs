module Modules.Roll
  ( respondToRoll )
  where

import           System.Random        (randomRIO)

import           BotAPI.Requests
import           BotAPI.Types.Chat
import           BotAPI.Types.Message
import           BotAPI.Types.User

import           Control.Arrow        ((>>>))
import           Control.Lens ((^.))
import           Data.Char
import           Data.Maybe
import           Text.Read            (readMaybe)

respondToRoll :: Message -> IO ()
respondToRoll msg = do
 rollResult <- roll args
 sendMessage chatId' (getRollMessage name rollResult args)
  where chatId' = msg ^. chat . chatId
        name = case msg ^. from of 
                    Nothing -> "UNKNOWN"
                    Just u -> u ^. firstName
        messageText = fromMaybe "" (msg ^. text)
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
