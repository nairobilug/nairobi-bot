module Bot.Network where

import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy as L
import Bot.Types

getJSON :: URL -> IO L.ByteString
getJSON url = do
  response <- get url
  return $ response ^. responseBody
