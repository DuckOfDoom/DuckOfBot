{-# OPTIONS_GHC -Wall #-}

module Modules.Hearthstone where

import HearthstoneAPI.Types (Card(..))
import HearthstoneAPI.Requests (searchCards)

import API.Requests (answerInlineQuery)
import API.Types.Inline (InlineQueryResult(..))
import Data.Maybe (mapMaybe)

respondToCardSearchQuery :: String -> String -> IO ()
respondToCardSearchQuery queryId queryText = do
    cards <- searchCards queryText
    _ <- answerInlineQuery queryId (mapMaybe cardToInlineQueryResult cards)
    return () 

cardToInlineQueryResult :: Card -> Maybe InlineQueryResult
cardToInlineQueryResult (Card cId _ _ (Just imageUrl) _) = Just (InlineQueryResult "photo" cId imageUrl imageUrl)
cardToInlineQueryResult _ = Nothing

