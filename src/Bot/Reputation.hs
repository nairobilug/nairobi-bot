{-# LANGUAGE Arrows #-}
module Bot.Reputation where

import Control.Auto
import Control.Auto.Blip
import Data.Map as M
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`

import Bot.Types

repBot :: Monad m => RoomBot m
repBot = proc (InMessage nick msg _ _) -> do
    updateB <- updateBlips -< (nick, msg)

    reps    <- trackReps   -< updateB

    queryB  <- queryBlips  -< msg

    let lookupRep :: Nick -> [Message]
        lookupRep nick' =
          case nick' of
           ""        -> ["Done."]
           _ -> [nick ++ " has a reputation of " ++ show rep ++ "."]
           where
             rep = M.findWithDefault 0 nick reps

    id -< lookupRep <$> queryB
  where
    updateBlips :: Auto m (Nick, Message) (Blip (Nick, Int))
    updateBlips = emitJusts getUpdateCommand
      where
        -- updater is the person triggering the update blip
        getUpdateCommand (updater, msg) =
          case words msg of
            "@addRep":nick:_ | nick /= updater -> Just (nick, 1)
            "@subRep":nick:_                   -> Just (nick, -1)
            _                                  -> Nothing
    trackReps :: Monad m => Auto m (Blip (Nick, Int)) (Map Nick Int)
    trackReps = scanB (\mp (nick, change) -> M.insertWith (+) nick change mp) M.empty
    queryBlips :: Auto m Message (Blip Nick)
    queryBlips = emitJusts (getRequest . words)
      where
        getRequest ("@addRep":_)   = Just ""
        getRequest ("@subRep":_)   = Just ""
        getRequest ("@rep":nick:_)  = Just nick
        getRequest _                = Nothing
