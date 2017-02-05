{-# LANGUAGE Arrows #-}
module Bot.Echo where

import Control.Auto
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Monad.IO.Class (MonadIO)
import Bot.Types


echoBot :: MonadIO m => RoomBot m
echoBot = proc (InMessage _ msg _ _) -> do

  echoB <- echoBlips -< msg

  -- | (: []) :: Message -> [Message]
  id -< (: []) <$> echoB
  where
    echoBlips :: Auto m Message (Blip Message)
    echoBlips = emitJusts (getRequest . words)
      where
        getRequest ("@echo": msg) = Just $ unwords msg
        getRequest _ = Nothing
