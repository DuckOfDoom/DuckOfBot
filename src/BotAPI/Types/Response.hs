{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Response
  ( Response(..)
  , ok
  , result
  , description
  , errorCode
  )
  where

import Control.Lens (makeLenses)
import Data.Aeson   (FromJSON(..), Value(..), parseJSON, (.:), (.:?))
import Data.Text    (Text)

data Response a = Response
  { _ok          :: Bool
  , _result      :: Maybe a
  , _description :: Maybe Text
  , _errorCode   :: Maybe Integer
  }
  deriving Show

makeLenses ''Response

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) =
    Response            <$>
    v .:  "ok"          <*>
    v .:? "result"      <*>
    v .:? "description" <*>
    v .:? "error_code"

  parseJSON _ = fail "Failed to parse Response object!"
