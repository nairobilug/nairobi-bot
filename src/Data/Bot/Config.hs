module Data.Bot.Config (getConfig) where

import Data.Yaml
import Bot.Types

import System.Environment (lookupEnv)



getConfig :: IO (Maybe Config)
getConfig = do
  conf <- decodeFile "config.yaml" :: IO (Maybe Config)
  case conf of
    Nothing -> env -- get config from environment variables.
    _ -> return conf

  where
    -- Fetch environment variables.
    env :: IO (Maybe Config)
    env = do
      network' <- lookupEnv "network"
      botName'  <- lookupEnv "name"
      channels' <- lookupEnv "channels"
      waId'     <- lookupEnv "wolframAlpha"
      lastFmId' <- lookupEnv "lastFm"
      let net     = case network' of
                         Just net' -> net'
                         Nothing -> error "The bot doesn't have a name.\
                                          \Set it in your config.yaml or an environment variable. \
                                          \If that fails report a bug at \
                                          \https://github.com/nairobilug/nairobi-bot/issues"
          botName = case botName' of
                          Just nm -> nm
                          Nothing -> error "The bot doesn't have a name.\
                                           \Set it in your config.yaml or an environment variable. \
                                           \If that fails report a bug at \
                                           \https://github.com/nairobilug/nairobi-bot/issues"
          chanLst = case channels' of
                      Just chanList -> chanList
                      Nothing -> error "The bot doesn't have a list of channels to join. \
                                        \Set it in your config.yaml or an environment variable. \
                                        \If that fails report a bug at \
                                        \https://github.com/nairobilug/nairobi-bot/issues"
          waId     = case waId' of
                       Just wId -> wId
                       Nothing -> ""

          lastFmId = case lastFmId' of
                       Just lId -> lId
                       Nothing -> ""

      return $ Just $ Config net botName [chanLst] waId lastFmId
