{-# OPTIONS_GHC -Wall #-}

module Modules.Countdown
  ( respondToLegionCountdown
  , respondToKharazhanCountdown
  ) where

import           API.Requests      (sendMessage)
import           API.Types.Message (Message, getMessageChatID)
import           Data.UTC
import           Util.Time         (getCurrentTime, makeDateTime, subtractDT)

legionReleaseTime :: DateTime
legionReleaseTime = makeDateTime 2016 8 30 0 0 0

kharazhanReleaseTime :: DateTime
kharazhanReleaseTime = makeDateTime 2016 8 11 0 0 0

respondToLegionCountdown :: Message -> IO ()
respondToLegionCountdown msg = sendCountdownMessage (getMessageChatID msg) "Вы будете готовы через " legionReleaseTime

respondToKharazhanCountdown :: Message -> IO ()
respondToKharazhanCountdown msg = sendCountdownMessage (getMessageChatID msg) "Вечеринка с любителем орчих через " kharazhanReleaseTime

sendCountdownMessage :: Integer -> String -> DateTime -> IO ()
sendCountdownMessage chatId message dateTime = do
 currTimeDateTime <- getCurrentTime
 sendMessage chatId (message ++ show (subtractDT dateTime currTimeDateTime))
