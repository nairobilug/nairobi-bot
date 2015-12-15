{-|
Module      : Bot.Help
Description : Give a help message to a user.
Copyright   : (c) 2015, Njagi Mwaniki 
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE Arrows #-}
module Bot.Help 
( helpBot
) where

import Control.Auto
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
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
    getRequest ("@help": _) = Just $ "Read the docs here: https://github.com/nairobilug/nairobi-bot/wiki#usage"
    getRequest _ = Nothing 
