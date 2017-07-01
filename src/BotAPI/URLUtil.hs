module BotAPI.URLUtil 
  ( meUrl
  , updatesUrl
  , messageUrl
  , photoUrl
  , answerInlineQueryUrl
  )
  where

import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import System.Exit (die)
import Data.Text (Text)
import qualified Data.Text as T (pack)

meUrl :: IO Text
meUrl = composeUrl "getMe"

updatesUrl :: IO Text
updatesUrl = composeUrl "getUpdates"

messageUrl :: IO Text
messageUrl = composeUrl "sendMessage"

photoUrl :: IO Text
photoUrl = composeUrl "sendPhoto"

answerInlineQueryUrl :: IO Text
answerInlineQueryUrl = composeUrl "answerInlineQuery"

readToken :: IO Text
readToken = do
  mToken <- lookupEnv "BOT_TOKEN"
  case mToken of 
    Just t  -> pure $ T.pack t
    Nothing -> die "Please assign token to 'BOT_TOKEN' environment variable!"

composeUrl :: Text -> IO Text
composeUrl url = do
  t <- readToken
  pure $ "https://api.telegram.org/bot" <> t <> "/" <> url
