module Modules.Default 
( respondToUnknown
, respondToPi
)
where

import BotAPI.Requests (sendMessage, sendPhoto)
import BotAPI.Types    (Message(..), chat, chatId, text)
import Control.Lens    ((^.))
import Data.Maybe      (fromMaybe)
import Data.Monoid     ((<>))
import Data.Text       as T (words)

respondToUnknown :: Message -> IO ()
respondToUnknown msg = sendMessage (msg ^. (chat . chatId)) ("Неизвестная команда: " <> cmd)
  where
    cmd = head $ T.words $ fromMaybe "" (msg ^. text)

respondToPi :: Message -> IO ()
respondToPi msg = sendPhoto (msg ^. (chat . chatId)) "pi.png"
