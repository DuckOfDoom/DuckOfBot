{-# LANGUAGE TemplateHaskell #-}

module BotAPI.Types.Response where

import           Data.Aeson
import Control.Lens

data Response a = Response
                { _ok          :: Bool
                , _result      :: Maybe a
                , _description :: Maybe String
                , _errorCode   :: Maybe Integer
                }
                deriving Show

makeLenses ''Response

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON (Object v) = Response <$>
                         v .: "ok" <*>
                         v .:? "result" <*>
                         v .:? "description" <*>
                         v .:? "error_code"
  parseJSON _ = fail "Failed to parse Response object!"
