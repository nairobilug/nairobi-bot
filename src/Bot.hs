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
-- import Data.Map                 (Map)
-- import Data.Serialize
import Data.Text hiding         (words, unwords, map)
import Data.Text.Encoding
-- import Data.Text.Encoding.Error
import Data.Time
import Network.SimpleIRC
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import qualified Data.Map       as M

-- Internal modules.
import Bot.Types
import Bot.Reputation
import Bot.Seen
import Bot.Echo
import Bot.NowPlaying
import Bot.Define

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

-- |Specific to freenode; bot name is "zippy-bot".
freenode :: IrcConfig
freenode = (mkDefaultConfig "irc.freenode.net" "zippy-bot")
            { cChannels = channels -- Channels to join on connect
            }

-- |The channels we want to the bot to join join.
channels :: [Channel]
channels = ["#zippy"]

chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ serializing' "rep.dat" $ perRoom repBot
                  , serializing' "seen.dat" $ perRoom seenBot
                  , perRoom echoBot
                  , perRoom defineBot
                  , serializing' "np.dat" $ perRoom npBot
                  ]

-- replace conf with freenode
-- bot is main
main :: IO ()
main = do
       withIrcConf freenode chatBot
       forever (threadDelay 1000000000)

{-
runOnChanM :: Monad m
           => (forall c. m c -> IO c)   -- convert `m` to `IO`
           -> (b -> IO Bool)            -- handle output
           -> Chan a                    -- chan to await input on
           -> Auto m a b                -- `Auto` to run
           -> IO (Auto m a b)
-}

perRoom :: Monad m => RoomBot m -> ChatBot m
perRoom rb = proc inp@(InMessage _ _ src _) -> do
    messages <- fromBlips [] . rb -< inp
    id -< OutMessages $ M.singleton src messages
