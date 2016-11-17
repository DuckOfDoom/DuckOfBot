{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Modules.HearthstoneEvents 
  ( respondToEventsRequest ) 
  where 

import           BotAPI.Requests      (sendMessage)
import BotAPI.Types (chatId,  Message, chat) 

import           Control.Lens         (makeLenses, (^.))
import           Data.Char            (isAlphaNum)
import           Data.List            (dropWhileEnd, intercalate)
import           Data.List.Split      (keepDelimsL, split, whenElt)
import           Data.Maybe           (mapMaybe)

import qualified Data.ByteString.Lazy as LBS (toStrict)
import           Data.Text            (unpack)
import qualified Data.Text.Encoding   as E (decodeUtf8)
import qualified Network.Wreq         as Wreq (get, responseBody)
import           Text.HTML.TagSoup    (Tag, fromAttrib, fromTagText,
                                       isTagOpenName, isTagText, maybeTagText,
                                       parseTags, renderTags, (~/=))

url :: String
url = "http://us.battle.net/hearthstone/en/fireside-gatherings?country=RU#fireside-gatherings"

type City = Maybe String
type State = Maybe String
type Country = Maybe String

data Location = Location Country State City

instance Show Location where 
  show (Location country state city) = pr country ++ " " ++ pr state ++ " " ++ pr city
    where pr (Just x) = x
          pr Nothing = ""

data Event = Event
           { _link     :: String
           , _name     :: Maybe String
           , _location :: Location
           , _date     :: Maybe String
           }

makeLenses ''Event

instance Show Event where
  show evt = "Event:\n" ++
             "    Link: " ++ show (evt ^. link) ++ "\n" ++
             "    Name: " ++ pr (evt ^. name) ++ "\n" ++
             "    Location: " ++ show (evt ^. location) ++ "\n" ++ 
             "    Date: " ++ pr (evt ^. date)
          where pr (Just x) = x
                pr Nothing = "N/A"

maybeTextAfterTag :: String -> [Tag String] -> Maybe String
maybeTextAfterTag tag allTags = fromNext $ dropWhile (~/= tag) allTags
  where fromNext xs = if length xs >= 2 then maybeTagText $ xs !! 1 else Nothing

isNotEmptyTagText :: Tag String -> Bool
isNotEmptyTagText t = isTagText t && any isAlphaNum (fromTagText t) || not (isTagText t)

getHtml :: IO String
getHtml = do
  response <- Wreq.get url
  return $ (unpack . E.decodeUtf8 . LBS.toStrict) (response ^. Wreq.responseBody)

findEventsTable :: String -> [Tag String]
findEventsTable = takeWhile (~/= "</div>") . drop 2 . dropWhile (~/= "</div>") . dropWhile (~/= "<div class=meetups-event-table>") . parseTags

splitEvents :: [Tag String] -> [[Tag String]]
splitEvents = drop 1 . split ((keepDelimsL . whenElt) isStartTag) . filter isNotEmptyTagText
  where isStartTag t = isTagOpenName "a" t && (fromAttrib "class" t == "meetups-event-table__row")

parseEvent :: [Tag String] -> Maybe Event
parseEvent tags | length tags < 3 = Nothing
                | (~/= "<a>") $ head tags = Nothing
                | otherwise = let eventLink = "http://us.battle.net/" ++ fromAttrib "href" (head $ dropWhile (~/= "<a>") tags)
                                  eventName = maybeTextAfterTag "<span class=meetups-event-table__cell__name>" tags
                                  eventLocation = Location country state city
                                    where country = maybeTextAfterTag "<span class=meetups-event-table__cell__country>" tags
                                          state = maybeTextAfterTag "<span class=meetups-event-table__cell__state>" tags
                                          city = maybeTextAfterTag "<span class=meetups-event-table__cell__city>" tags
                                  eventDate = parseDate <$> maybeTextAfterTag "<span class=\"meetups-event-table__cell meetups-event-table__cell--time\">" tags
                                    where parseDate = dropWhile (not . isAlphaNum) . dropWhileEnd (not . isAlphaNum)
                               in Just (Event eventLink eventName eventLocation eventDate)

respondToEventsRequest :: Message -> IO ()
respondToEventsRequest msg = do
  eventTags <- (splitEvents . findEventsTable) <$> getHtml
  sendMessage (chatId $ chat msg) (intercalate "\n\n" $ map show $ mapMaybe parseEvent eventTags)
