{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.Define where

import Data.Aeson (decode)
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto

import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Auto.Effects (arrMB)
import qualified Data.ByteString.Lazy as LB
import Data.Text (unpack)

import Bot.Data.Network
import Bot.Types



defineBot :: MonadIO m => RoomBot m
defineBot = proc (InMessage _ msg _ _) -> do
  -- | Check whether request is a definition request.
  blipRequest <- emitJusts getRequest -< msg

  blipDef <- arrMB (liftIO . getDefine) -< blipRequest

  id -< (: []) <$> blipDef
  where
    getRequest :: Message -> Maybe Message
    getRequest msg' =
      case words msg' of
        ("@define": xs)  -> Just $ concat $ intersperse "+" xs
        _                -> Nothing

    showMaybeDef :: Maybe Definition -> String
    showMaybeDef (Just (Definition "" def)) = unpack def
    showMaybeDef (Just (Definition def' _)) = unpack def'
    showMaybeDef _ =
      "Not found. You might want to report a bug at: https://github.com/urbanslug/nairobi-bot/issues"

    getDefine :: Query -> IO Message
    getDefine query =
      let url = "http://api.duckduckgo.com/?q="++query++"&format=json"
      in do
        eitherResponse <- safeGet url
        case eitherResponse of
          Left ex -> return $ "Define failed due to " ++ unpack ex
          Right resp -> return $ showMaybeDef $ decode $ LB.fromStrict $ body $ parseResponse resp
