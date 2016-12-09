module Modules.Countdown
  ( respondToLegionCountdown
  ) where

import           BotAPI.Requests      (sendMessage)
import           BotAPI.Types.Message (Message, getMessageChatID)
import           Data.UTC
import           Util.Time         (getCurrentTime, makeDateTime, subtractDT)

legionReleaseTime :: DateTime
legionReleaseTime = makeDateTime 2016 8 30 0 0 0

respondToLegionCountdown :: Message -> IO ()
respondToLegionCountdown msg = sendCountdownMessage (getMessageChatID msg) "Вы будете готовы через " legionReleaseTime

sendCountdownMessage :: Integer -> String -> DateTime -> IO ()
sendCountdownMessage chatId message dateTime = do
 currTimeDateTime <- getCurrentTime
 sendMessage chatId (message ++ show (subtractDT dateTime currTimeDateTime))
