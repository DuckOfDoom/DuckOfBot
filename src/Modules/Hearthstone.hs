{-# OPTIONS_GHC -Wall #-}

module Modules.Hearthstone 
  ( respondToCardSearchQuery )
  where

import           HearthstoneAPI.Requests (searchCards)
import           HearthstoneAPI.Types    (Card (..))

import           API.Requests            (answerInlineQuery)
import           API.Types.Inline        (InlineQueryResult (..))
import           Data.Maybe              (mapMaybe)

respondToCardSearchQuery :: String -> String -> IO ()
respondToCardSearchQuery queryId queryText = do
    putStrLn queryText
    cards <- searchCards (dropWhile (== ' ') queryText)
    answerInlineQuery queryId (cardsToResults cards)

cardsToResults :: [Card] -> [InlineQueryResult]
cardsToResults = take 50 . mapMaybe toResult -- We take only 50 cards that contain pictures for results, since Telegram API allows max 50 results per query
  where toResult (Card cId _ _ (Just imageUrl) _) = Just (InlineQueryResult "photo" cId imageUrl imageUrl)
        toResult _ = Nothing
