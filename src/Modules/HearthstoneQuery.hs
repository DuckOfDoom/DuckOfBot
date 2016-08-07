{-# OPTIONS_GHC -Wall #-}

module Modules.Hearthstone where

import HearthstoneAPI.Types (Card)
import HearthstoneAPI.Requests (searchCards)

import API.Types.Inline (InlineQuery, InlineQueryResult)

respondToCardSearchQuery :: String -> String -> IO ()
respondToCardSearchQuery queryId queryText = do
    putStrLn queryText
    cards <- searchCards (dropWhile (== ' ') queryText)
    _ <- answerInlineQuery queryId (mapMaybe cardToInlineQueryResult cards)
    return () 

cardToInlineQueryResult :: Card -> Maybe I.InlineQueryResult
cardToInlineQueryResult (Card cardId _ _ (Just imageUrl) _) = Just (I.InlineQueryResult "photo" cardId imageUrl imageUrl)
cardToInlineQueryResult _ = Nothing
