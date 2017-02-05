{-# LANGUAGE Arrows #-}
module Bot.Help
( helpBot
) where

import Control.Auto
import Prelude hiding ((.), id)
import Control.Monad.IO.Class (MonadIO)
import Bot.Types


helpBot :: MonadIO m => RoomBot m
helpBot = proc (InMessage _ msg _ _) -> do

  echoB <- getHelp -< msg

  -- | (: []) :: Message -> [Message]
  id -< (: []) <$> echoB
  where
    getHelp :: Auto m Message (Blip Message)
    getHelp = emitJusts (getRequest . words)

    getRequest :: [String] -> Maybe Message
    getRequest ("@help": _) =
      Just $ "Read the docs: https://github.com/nairobilug/nairobi-bot"
    getRequest _ = Nothing
