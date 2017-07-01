module BotAPI.Processing
  ( start )
  where

import qualified BotAPI.Requests as Requests
import           BotAPI.Types    (Response(..), Update(..), firstName,
                                  result, updateId)

import Control.Concurrent (threadDelay)
import Control.Lens       ((^.))
import Control.Monad      (when)
import Data.Either        (isRight)
import Data.Monoid        ((<>))
import Data.Text          (Text)

-- TODO: Fix printme without isRight
start :: Float -> (Update -> IO ()) -> IO ()
start delay processUpdate = do
  putStrLn "Hello there! Let me check something..."
  me <- Requests.getMe
  print $ printMe me
  when (isRight me) (startGetUpdatesLoopWithDelay delay processUpdate)
  where
    printMe (Left e) = "Whoops! Something went wrong: '" <> e <> "'"
    printMe (Right r) = "All is good! Nice to meet you! My name is: " <> getUsername r
    getUsername resp = case resp ^. result of
      Just u  -> u ^. firstName
      Nothing -> "UNKNOWN"


startGetUpdatesLoopWithDelay :: Float -> (Update -> IO ()) -> IO ()
startGetUpdatesLoopWithDelay delay processUpdate = do
  response <- Requests.getUpdates
  lastUpdateId <- processUpdatesAndReturnLastId response processUpdate
  getUpdatesLoop delay lastUpdateId
    where
      getUpdatesLoop d lastId = do
        response <- Requests.getUpdatesWithId (lastId + 1)
        newId <- processUpdatesAndReturnLastId response processUpdate
        threadDelay $ truncate (1000000 * delay)
        getUpdatesLoop d newId

processUpdatesAndReturnLastId :: Either Text (Response [Update]) -> (Update -> IO ()) -> IO Integer
processUpdatesAndReturnLastId (Left ex) _ = print ("[Bot] Failed to process updates: " <> ex) >> return 0
processUpdatesAndReturnLastId (Right (Response _ Nothing _ _)) _ = return 0
processUpdatesAndReturnLastId (Right (Response _ (Just []) _ _)) _ = return 0
processUpdatesAndReturnLastId (Right (Response _ (Just updates) _ _)) processUpdate = do
  mapM_ processUpdate updates
  return $ last updates ^. updateId
