{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module BotAPI.Bot 
  ( start )
  where

import BotAPI.Requests
import qualified BotAPI.Types.Update as U 
import qualified BotAPI.Types.Response as R
import qualified BotAPI.Types.User as User

import           Data.Either           (isRight)
import           Control.Monad         (when)
import           Data.Maybe            (fromJust)
import           Control.Concurrent    (threadDelay)

start :: Float -> (U.Update -> IO ()) -> IO ()
start delay processUpdate = do 
  putStrLn "Hello there! Let me check something..."
  me <- getMe
  putStrLn $ printMe me
  when (isRight me) (startGetUpdatesLoopWithDelay delay processUpdate) 
  where printMe (Left e) = "Whoops! Something went wrong: '" ++ e ++ "'"
        printMe (Right (R.Response _ user _ _ )) = "All is good! Nice to meet you! My name is: " ++ User.firstName (fromJust user)

startGetUpdatesLoopWithDelay :: Float -> (U.Update -> IO ()) -> IO ()
startGetUpdatesLoopWithDelay delay processUpdate = do
  response <- getUpdates
  lastUpdateId <- processUpdatesAndReturnLastId response processUpdate
  getUpdatesLoop delay lastUpdateId
    where getUpdatesLoop d lastId = do
            response <- getUpdatesWithId (lastId + 1)
            newId <- processUpdatesAndReturnLastId response processUpdate
            threadDelay $ truncate (1000000 * delay)
            getUpdatesLoop d newId

processUpdatesAndReturnLastId :: Either String (R.Response [U.Update]) -> (U.Update -> IO ()) -> IO Integer
processUpdatesAndReturnLastId (Left ex) _ = putStrLn ("[Bot] Failed to process updates: " ++ ex) >> return 0
processUpdatesAndReturnLastId (Right (R.Response _ Nothing _ _)) _ = return 0
processUpdatesAndReturnLastId (Right (R.Response _ (Just []) _ _)) _ = return 0
processUpdatesAndReturnLastId (Right (R.Response _ (Just updates) _ _)) processUpdate = do
  mapM_ processUpdate updates
  return $ (U.updateId . last) updates
