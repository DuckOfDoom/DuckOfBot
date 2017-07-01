module HearthstoneAPI.Util
  ( getSearchUrl
  , readToken
  , getQueryLocale
  ) where

import Data.Text (Text)
import qualified Data.Char as C (toLower)
import qualified Data.Text as T (pack, dropWhile, head, null)
import System.Environment (lookupEnv)

readToken :: IO Text
readToken = do
  mToken <- lookupEnv "HEARTHSTONE_TOKEN"
  case mToken of 
    Just t -> pure $ T.pack t
    Nothing -> do 
      print ("Please assign Hearthstone API token to 'HEARTHSTONE_TOKEN' environment variable!" :: Text)
      pure "BOT_TOKEN"

-- We need to find out what card language user is trying to get.
-- I didnt figure out a better way yet
getQueryLocale :: Text -> Text
getQueryLocale s 
  | T.null s = "enGB"
  | ch `elem` ['a'..'z'] = "enGB"
  | ch `elem` ['а'..'я'] = "ruRU"
  | otherwise = "enGB"
  where 
    ch :: Char
    ch = C.toLower . T.head . T.dropWhile (== ' ') $ s

baseUrl :: Text
baseUrl = "https://omgvamp-hearthstone-v1.p.mashape.com/cards"

-- API endpoint includes card name as part of adress, not a param
getSearchUrl :: Text -> Text
getSearchUrl name = mconcat [baseUrl, "/search/", name]
