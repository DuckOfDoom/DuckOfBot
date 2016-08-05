{-# OPTIONS_GHC -Wall #-}

module Modules.Countdown where

import           API.Requests      (sendMessage)
import           API.Types.Message (Message, getMessageChatID)
import           Data.UTC
import           Util.Time         (getCurrentTime, makeDateTime, subtractDT)

legionReleaseTime :: DateTime
legionReleaseTime = makeDateTime 2016 8 30 0 0 0

respondToLegionCountdown :: Message -> IO ()
respondToLegionCountdown msg = do
 currTime <- getCurrentTime
 sendMessage (getMessageChatID msg) ("Терпение! Осталось всего " ++ show (subtractDT legionReleaseTime currTime))
