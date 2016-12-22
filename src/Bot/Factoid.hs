{-# LANGUAGE Arrows, InstanceSigs #-}
module Bot.Factoid where

import Control.Auto
import Data.Map as M
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Data.List as L
import Control.Monad.IO.Class
import Control.Auto.Blip

import Bot.Types

newtype Factoid = Factoid (String, String)

factoidBot :: MonadIO m => RoomBot m
factoidBot = proc (InMessage _ msg _ _) -> do

    blipAddFactoidRequest  <- emitJusts getAddFactoidRequest -< msg

    factoids <- addFactoid -< blipAddFactoidRequest

    let registeredMessage = "Factoid added ^_^" <$ blipAddFactoidRequest

    queryB <- emitJusts (search . queryFactoid)  -< (msg, factoids)

    id -< (: []) <$> (queryB `mergeL` registeredMessage)
  where
    getAddFactoidRequest :: Message -> Maybe (String, [String])
    getAddFactoidRequest msg' =
      case words msg' of
        (x:factoid:value)
          | x == "@factoid" -> Just (factoid, value)
          | otherwise       -> Nothing
        _ -> Nothing

    addFactoid :: Auto m (Blip (String, [String])) (M.Map String [String])
    addFactoid = scanB (\acum (factoid, value) -> M.insert factoid value acum) M.empty

    queryFactoid :: (String, M.Map String [String]) -> Maybe (String, M.Map String [String])
    queryFactoid (msg', factoids') =
      case words msg' of
        [(x:factoid)]
          | x == '!'   -> Just (factoid, factoids')
          | otherwise  -> Nothing
        _ -> Nothing

    search :: Maybe (String, M.Map String [String]) -> Maybe String
    search (Just (factoid', factoids')) =
      case M.lookup factoid' factoids' of
        Just value -> Just $ L.intercalate " " value
        Nothing    -> Just $ "Factoid " ++ factoid' ++ " not found"
    search _ = Nothing
