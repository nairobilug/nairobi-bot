{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.GIF where

import Data.Aeson (decode)
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto
import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Auto.Effects (arrMB)
import qualified Data.ByteString.Lazy as LB
import Data.Text (unpack)
import Bot.Types
import Bot.Data.Network


gifBot :: MonadIO m => RoomBot m
gifBot = proc (InMessage _ msg _ _) -> do
  blipRequest <- emitJusts getRequest -< msg

  blipDef <- arrMB (liftIO . getGIF) -< blipRequest

  id -< (: []) <$> blipDef
  where
    getRequest :: Message -> Maybe Message
    getRequest msg' =
      case words msg' of
        ("@gif": xs)  -> Just $ concat $ intersperse "+" xs
        _             -> Nothing

    showMaybeGIF :: Maybe GIF -> URL
    showMaybeGIF (Just (GIF url)) = url
    showMaybeGIF _ = "Not found. You might want to report a bug at:"
                     ++ "https://github.com/urbanslug/nairobi-bot/issues"

    getGIF :: Query -> IO Message
    getGIF q = do
      eitherResponse <- safeGet $
        "http://api.giphy.com/v1/gifs/search?q="++q++"&api_key=dc6zaTOxFJmzC"
      case eitherResponse of
        Left ex -> return $ "Fetching GIF failed due to " ++ unpack ex
        Right resp -> return $ showMaybeGIF $ decode $ body $
                               parseResponse resp
