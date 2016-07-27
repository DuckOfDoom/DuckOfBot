{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- A shortcut module for all JSON objects
module API.Types
  ( module T )
  where

import           API.Types.Chat     as T
import           API.Types.Inline   as T
import           API.Types.Message  as T
import           API.Types.Response as T
import           API.Types.Update   as T
import           API.Types.User     as T
