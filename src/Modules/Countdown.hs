{-# OPTIONS_GHC -Wall #-}

module Modules.Countdown where 

import Data.UTC
import Data.Maybe
import Data.Ratio ((%), numerator)

legionReleaseTime :: DateTime
legionReleaseTime = getDateTime 2016 8 30 0 0 0

printCurrentYear :: IO ()
printCurrentYear
  = do now <- getUnixTime :: IO DateTime
       print (year now)

getDateTime :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> DateTime
getDateTime y m d h mi sec = fromJust $ setYear y (epoch :: DateTime) 
                                                >>= setMonth m 
                                                >>= setDay d 
                                                >>= setHour h
                                                >>= setMinute mi
                                                >>= setSecond sec

getTimespan :: DateTime -> DateTime -> DateTime
getTimespan fst snd = secondsToDateTime (dateTimeToSeconds fst - dateTimeToSeconds snd)

secondsToDateTime :: Integer -> DateTime
secondsToDateTime s = fromJust (fromUnixSeconds (s % 1) :: Maybe DateTime)

dateTimeToSeconds :: DateTime -> Integer
dateTimeToSeconds dt = numerator (unixSeconds dt) 
