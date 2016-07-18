{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Args where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)

{-# ANN module "HLint: ignore Use camelCase" #-}

data SendMessageArgs = SendMessageArgs
                     { chat_id :: String
                     , text    :: String
                     }
                     deriving (Generic, Show)

instance ToJSON SendMessageArgs
