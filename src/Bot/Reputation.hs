{-|
Module      : Bot.Reputation
Description : Give people points or subtruct them.
Copyright   : (c) 2015, Njagi Mwaniki
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE Arrows #-}
module Bot.Reputation where

import Control.Auto
import Control.Auto.Blip
import Data.Map as M
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`

import Bot.Types


repBot :: Monad m => RoomBot m
repBot = proc (InMessage nick msg _ _) -> do

    queryB  <- queryBlips  -< (nick, msg)

    reps    <- trackReps   -< queryB

    let lookupRep :: (Nick, Int) -> [Message]
        lookupRep (nick', _) =
          case nick' of
           "Insult"        -> ["Operation not allowed, dumbass!"]
           _ -> [nick' ++ " has a reputation of " ++ show rep ++ "."]
           where
             rep = M.findWithDefault 0 nick' reps

    id -< lookupRep <$> queryB
  where
    queryBlips :: Auto m (Nick, Message) (Blip (Nick, Int))
    queryBlips =  emitJusts getRequest
      where
        getRequest (nick, msg) =
          case words msg of
              "+1":nick':_ -> if nick /= nick'
                               then Just (nick', 1)
                               else Just ("Insult", 0)
              "-1":nick':_ ->  if nick /= nick'
                                 then Just (nick', -1)
                                 else Just ("Insult", 0)
              "@rep":nick':_    -> Just (nick', 0)
              _         -> Nothing

    trackReps :: Auto m (Blip (Nick, Int)) (Map Nick Int)
    trackReps = scanB (\mp (nick, change) -> M.insertWith (+) nick change mp) M.empty
