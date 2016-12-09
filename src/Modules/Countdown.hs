module Modules.Countdown
  ( respondToLegionCountdown
  ) where

import           BotAPI.Requests      (sendMessage)
import           BotAPI.Types.Chat
import           BotAPI.Types.Message
import           Data.UTC
import           Util.Time         (getCurrentTime, makeDateTime, subtractDT)
import Control.Lens

legionReleaseTime :: DateTime
legionReleaseTime = makeDateTime 2016 8 30 0 0 0

respondToLegionCountdown :: Message -> IO ()
respondToLegionCountdown msg = sendCountdownMessage (msg ^. (chat . chatId)) "Вы будете готовы через " legionReleaseTime

sendCountdownMessage :: Integer -> String -> DateTime -> IO ()
sendCountdownMessage chatId' message dateTime = do
 currTimeDateTime <- getCurrentTime
 sendMessage chatId' (message ++ show (subtractDT dateTime currTimeDateTime))
