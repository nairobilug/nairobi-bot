{-# LANGUAGE Arrows #-}
module Bot.Ping where

import Control.Auto
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Monad.IO.Class (MonadIO)

import Bot.Types



pingBot :: MonadIO m => RoomBot m
pingBot = proc (InMessage _ msg _ _) -> do

  echoB <- echoBlips -< msg

  -- | (: []) :: Message -> [Message]
  id -< (: []) <$> echoB
  where
    echoBlips :: Auto m Message (Blip Message)
    echoBlips = emitJusts (getRequest . words)
      where
        getRequest ("@ping": _) = Just "PONG!"
        getRequest _            = Nothing
