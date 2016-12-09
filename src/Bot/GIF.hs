{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.GIF where

import Data.Aeson (decode)
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto

import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Auto.Effects (arrMB)
import qualified Data.ByteString.Lazy as L

import Bot.Network
import Bot.Types



gifBot :: MonadIO m => RoomBot m
gifBot = proc (InMessage _ msg _ _) -> do
  -- | Check whether request is a definition request.
  blipRequest <- emitJusts getRequest -< msg

  blipDef <- arrMB (liftIO . getDefine) -< blipRequest

  id -< (: []) <$> blipDef
  where
    getRequest :: Message -> Maybe Message
    getRequest msg' =
      case words msg' of
        ("@gif": xs)  -> Just $ concat $ intersperse "+" xs
        _             -> Nothing

    showMaybeGIF :: Maybe GIF -> URL
    showMaybeGIF (Just (GIF url)) = url
    showMaybeGIF _ = "Not found. You might want to report a bug at: https://github.com/urbanslug/nairobi-bot/issues"

    getDefine :: Query -> IO Message
    getDefine query =
      let decode' = decode :: L.ByteString -> Maybe GIF
          url = "http://api.giphy.com/v1/gifs/search?q="++query++"&api_key=dc6zaTOxFJmzC"
      in do
        eitherJSON <- getWebPage url -- fmap (showMaybeDef . decode') $ getJSON url
        case eitherJSON of
          Right json -> return $ (showMaybeGIF . decode') json
          Left ex -> return $ "Define failed due to " ++ ex
