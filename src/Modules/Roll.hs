module Modules.Roll
  ( respondToRoll )
  where

import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Char     as C
import           System.Random (randomRIO)

import BotAPI.Requests
import BotAPI.Types.Chat
import BotAPI.Types.Message
import BotAPI.Types.User

import Control.Arrow ((>>>))
import Control.Lens  ((^.))
import Data.Maybe
import Text.Read     (readMaybe)

respondToRoll :: Message -> IO ()
respondToRoll msg = do
 rollResult <- roll args
 sendMessage chatId' (getRollMessage name rollResult args)
  where chatId' = msg ^. chat . chatId
        name = case msg ^. from of
                    Nothing -> "UNKNOWN"
                    Just u  -> u ^. firstName
        messageText = fromMaybe "" (msg ^. text)
        args = readArgs messageText

getRollMessage :: Text -> Int -> (Int, Int) -> Text
getRollMessage name result (l, u) =
  mconcat [name, " выбрасывает ", T.pack $ show result, " (", T.pack $ show l, "-", T.pack $ show u, ")"]

roll :: (Int, Int) -> IO Int
roll (l, u) | u < l = roll (u, l)
            | otherwise = randomRIO (l, u)

readArgs :: Text -> (Int, Int)
readArgs = takeTuple >>> replaceDash >>> readResult >>> fromMaybe (1, 100)
  where
    takeTuple = T.dropWhile (not . C.isDigit) >>> T.takeWhile (\s -> (C.isDigit s) || (s == '-') || (s == ' '))
    replaceDash = T.replace "-" ","
    readResult x = readMaybe $ T.unpack $ mconcat ["(", x, ")"] :: Maybe (Int, Int)
