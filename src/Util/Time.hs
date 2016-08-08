{-# OPTIONS_GHC -Wall #-}

module Util.Time where

import           Data.Maybe
import           Data.UTC
import           Text.Printf (printf)

data Timespan = Timespan { days    :: Integer
                         , hours   :: Integer
                         , minutes :: Integer
                         , seconds :: Integer
                         }

instance Show Timespan where
  show t = printf "%dд %dч %dм %dс" (days t) (hours t) (minutes t) (seconds t)

fromSeconds :: Integer -> Timespan
fromSeconds x = Timespan d h m s
  where d = x `div` 86400
        h = (x - d * 86400) `div` 3600
        m = (x - d * 86400 - h * 3600) `div` 60
        s = x - d * 86400 - h * 3600 - m * 60

getCurrentTime :: IO DateTime
getCurrentTime = getUnixTime :: IO DateTime

makeDateTime :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> DateTime
makeDateTime y m d h mi sec = fromJust $ setYear y (epoch :: DateTime)
                                                >>= setMonth m
                                                >>= setDay d
                                                >>= setHour h
                                                >>= setMinute mi
                                                >>= setSecond sec

subtractDT :: DateTime -> DateTime -> Timespan
subtractDT t1 t2 = fromSeconds (dateTimeToSeconds t1 - dateTimeToSeconds t2)

dateTimeToSeconds :: DateTime -> Integer
dateTimeToSeconds dt = truncate (unixSeconds dt) :: Integer
