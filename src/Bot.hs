{-|
Module      : Bot
Description : The Bot itself.
Copyright   : (c) 2015, Njagi Mwaniki 
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot where

import Control.Auto
import Control.Concurrent       (Chan, newChan, writeChan, forkIO, threadDelay)
import Control.Auto.Run         (runOnChanM)
import Control.Auto.Serialize   (serializing')
import Control.Monad            (void, forever)
import Control.Monad.IO.Class
import Data.Foldable            (forM_)
import Data.Text hiding         (words, unwords, map)
import Data.Text.Encoding
import Data.Time
import Network.SimpleIRC
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import qualified Data.Map       as M

import Bot.Types
import Bot.Reputation
import Bot.Wolfram
import Bot.Seen
import Bot.Echo
import Bot.NowPlaying
import Bot.Define
import Data.Bot.Config
import Bot.URL
import Bot.Help
import Bot.Meetup

withIrcConf :: IrcConfig -> ChatBot IO -> IO ()
withIrcConf ircconf chatbot = do

    -- chan to receive `InMessages`
    inputChan <- newChan :: IO (Chan InMessage)

    -- configuring IRC
    let events   = cEvents ircconf ++ [ Privmsg (onMessage inputChan) ]
        ircconf' = ircconf { cEvents = events } -- ^ :: IrcConfig

    -- connect; simplified for demonstration purposes
    Right server <- connect ircconf' True True

    -- run `chatbot` on `inputChan`
    void . forkIO . void $
        runOnChanM id (processOutput server) inputChan chatbot

  where
    -- what to do when `chatBot` outputs
    processOutput :: MIrc -> OutMessages -> IO Bool
    processOutput server (OutMessages outs) = do
      print outs
      _ <- flip M.traverseWithKey outs $ \channel messages -> do
        let channel' = encodeUtf8 . pack $ channel
        forM_ messages $ \message -> do
          let message' = encodeUtf8 . pack $ message
          sendMsg server channel' message'
      return True       -- "yes, continue on"

    -- what to do when you get a new message
    onMessage :: Chan InMessage -> EventFunc
    onMessage inputChan = \_ message -> do
      case (mNick message, mOrigin message) of
        (Just nick, Just src) -> do
          time <- getCurrentTime
          writeChan inputChan $ InMessage (unpack (decodeUtf8 nick))
                                          (unpack (decodeUtf8 (mMsg message)))
                                          (unpack (decodeUtf8 src))
                                          time
        (Just _, Nothing)  -> undefined
        (Nothing, Just _)  -> undefined
        (Nothing, Nothing) -> undefined

-- |Specific to freenode; bot name is "nairobi-bot".
freenode :: String   -- | network
         -> String   -- | name
         -> [String] -- | channel list
         -> IrcConfig
freenode network' botName chans = (mkDefaultConfig network' botName)
                        { cChannels = (channels' chans) -- Channels to join on connect
                        }

-- |The channels we want to the bot to join join.
channels' :: [String] -- | Channel list from yaml file without hashes.
          -> [Channel]
channels' chans = map ('#':) chans

chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ serializing' "rep.dat" $ perRoom repBot
                  , serializing' "seen.dat" $ perRoom seenBot
                  , perRoom echoBot
                  , perRoom defineBot
                  , perRoom waBot
                  , perRoom urlBot
                  , perRoom helpBot
                  , perRoom meetupBot
                  , serializing' "np.dat" $ perRoom npBot
                  ]

-- replace conf with freenode
-- bot is main
main :: IO ()
main = do
       conf <- getConfig
       -- A failure of configs here means a failure in the entire bot.
       config <-
         case conf of
           Just c  -> return c
           Nothing -> fail "Can't find bot name or channel list.\
                           \Fix your config.yaml \
                           \if that fails report a bug at \
                           \https://github.com/nairobilug/nairobi-bot/issues"
       let botName = name config
           network' = network config
           channelList = channels config
       withIrcConf (freenode network' botName channelList) chatBot
       forever (threadDelay 1000000000)

perRoom :: Monad m => RoomBot m -> ChatBot m
perRoom rb = proc inp@(InMessage _ _ src _) -> do
    messages <- fromBlips [] . rb -< inp
    id -< OutMessages $ M.singleton src messages
