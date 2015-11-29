{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.NowPlaying where

import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto
import Control.Monad.IO.Class (liftIO, MonadIO)
import Network
import Data.Aeson (decode)
import Control.Auto.Effects (arrMB)
import Control.Auto.Blip 
import qualified Data.Map as M
import Data.Text (unpack)
import System.Environment (getEnv)

import Bot.Types
import Bot.Network

npBot :: MonadIO a => RoomBot a
npBot = proc (InMessage nick msg _ _) -> do

  -- | When user requests to set a username.
  blipRegisterRequest <- emitJusts getRegisterRequest -< (nick, msg)

  -- | We build a map of usernames and nicks here.
  usernames <- foldMap' -< blipRegisterRequest

  let registeredMessage = "last.fm username registered ^_^" <$ blipRegisterRequest

  -- | We get the username or nick to use here.
  blipNick <- emitJusts (search . getRequest) -< (nick, msg, usernames)

  -- | We get the currently playing song.
  np <- arrMB (liftIO . getNP) -< blipNick

  -- | Output to the user.
  id -< (: []) <$> (np `mergeL` registeredMessage)
  where
    search :: (Maybe Nick, Maybe String, (M.Map Nick Username)) -> Maybe (Username, Nick)
    search ((Just nick), Nothing, usernames') = 
      case M.lookup nick usernames' of
        Just uname -> Just (uname, nick)
        _          -> Just (nick, nick)
    search ((Just nick), (Just uname), _) = 
      Just (uname, nick)
    search _ = Nothing

    foldMap' :: Auto m (Blip (Nick, Username)) (M.Map Nick Username)
    foldMap' = 
      scanB (\acum (nick, username) -> M.insert nick username acum) M.empty

    getRegisterRequest :: (Nick, Message) -> Maybe (Nick, Username)
    getRegisterRequest (nick, msg') = 
      case words msg' of
        ["@np", "set", uName'] -> Just (nick, uName')
        _                       -> Nothing

    getRequest :: (Nick, Message, M.Map Nick Username) 
                  -> (Maybe Nick, Maybe String, M.Map Nick Username)
    getRequest (nick , msg', map') = 
      case words msg' of
        ["@np"]  -> (Just nick, Nothing, map')
        ["@np", uname] -> (Just nick ,Just uname, map')
        _ -> (Nothing, Nothing, map')

    showMaybeNP :: Maybe NowPlaying -> String
    showMaybeNP (Just (NowPlaying song' artist' album')) = 
      " is listeing to \"" ++ (unpack song') ++ "\" by " 
      ++ (unpack artist') ++ " from the album " 
      ++ (unpack album') ++ "."
    showMaybeNP Nothing = 
      "Not found. Please report a bug at: https://github.com/urbanslug/nairobi-bot/issues"
      
    
    getNP :: (Username, Nick) -> IO Message
    getNP (uname, nick) = do
      appId <- getEnv "lastfm_appid"
      json  <- getJSON $ "http://ws.audioscrobbler.com/2.0/?method=user.getRecentTracks&user=" 
                         ++ uname ++ "&api_key="++ appId ++"&limit=1&format=json"
      return $ ( ((++) nick) . showMaybeNP . decode) json
