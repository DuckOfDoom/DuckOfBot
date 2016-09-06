{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- A shortcut module for all JSON objects
module BotAPI.Types
  ( module T )
  where

import           BotAPI.Types.Chat     as T
import           BotAPI.Types.Inline   as T
import           BotAPI.Types.Message  as T
import           BotAPI.Types.Response as T
import           BotAPI.Types.Update   as T
import           BotAPI.Types.User     as T
