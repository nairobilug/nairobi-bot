{-# LANGUAGE Arrows, OverloadedStrings, RecordWildCards #-}
module Bot where

import Control.Auto
import Control.Concurrent     (Chan, newChan, writeChan, forkIO, threadDelay)
import Control.Auto.Run       (runOnChanM)
import Control.Auto.Serialize (serializing')
import Control.Monad          (void, forever)
import Control.Monad.IO.Class
import Data.Foldable          (forM_)
import Data.Text hiding       (words, unwords, map)
import Data.Text.Encoding
import Data.Time
import Network.SimpleIRC
import Prelude hiding         ((.), id) -- we use (.) and id from `Control.Category`
import qualified Data.Map.Lazy as M
import Control.Exception (catch)

import Bot.Types
import Bot.Reputation
import Bot.Wolfram
import Bot.Seen
import Bot.Echo
import Bot.NowPlaying
import Bot.Define
import Bot.Data.Config
import Bot.URL
import Bot.Help
import Bot.Ping
import Bot.GIF
import Bot.Factoid


configFail :: String
configFail = "Unexpected error from getConfig"

main :: IO ()
main = do
  config <- catch (getConfig "config.yml")
                  unexpectedConfigError -- Throw a custom error.
  withIrcConf (genIrcConfig config) chatBot
  forever (threadDelay 1000000000)

  where unexpectedConfigError :: IOError -> IO Config
        unexpectedConfigError ex =
          ioError $ userError $ configFail ++ "Additional info: " ++  (show ex)

genIrcConfig :: Config  -> IrcConfig
genIrcConfig Config {..} = (mkDefaultConfig server name) {cChannels = channels}

chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ serializing' "rep.dat"     $ perRoom repBot
                  , serializing' "seen.dat"    $ perRoom seenBot
                  , serializing' "np.dat"      $ perRoom npBot
                  , serializing' "factoid.dat" $ perRoom factoidBot
                  , perRoom echoBot
                  , perRoom defineBot
                  , perRoom waBot
                  , perRoom urlBot
                  , perRoom helpBot
                  , perRoom pingBot
                  , perRoom gifBot
                  ]

perRoom :: Monad m => RoomBot m -> ChatBot m
perRoom rb = proc inp@InMessage {..} -> do
  blipOutput <- rb  -< inp
  output <- fromBlips [] -< blipOutput
  id -< OutMessages $ M.singleton channel output

withIrcConf :: IrcConfig -> ChatBot IO -> IO ()
withIrcConf ircconf chatbot = do

    -- chan to receive `InMessages`
    inputChan <- newChan :: IO (Chan InMessage)

    -- configuring IRC
    let events   = cEvents ircconf ++ [ Privmsg (onMessage inputChan) ]
        ircconf' = ircconf { cEvents = events } -- ^ :: IrcConfig

    -- connect; simplified for demonstration purposes
    Right server' <- connect ircconf' True True

    -- run `chatbot` on `inputChan`
    void . forkIO . void $
      runOnChanM id (processOutput server') inputChan chatbot

  where
    -- what to do when `chatBot` outputs
    processOutput :: MIrc -> OutMessages -> IO Bool
    processOutput server' (OutMessages outs) = do
      print outs
      _ <- flip M.traverseWithKey outs $ \channel messages -> do
        let channel' = encodeUtf8 . pack $ channel
        forM_ messages $ \message -> do
          let message' = encodeUtf8 . pack $ message
          sendMsg server' channel' message'
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
        (Just _ , Nothing) -> undefined
        (Nothing, Just _)  -> undefined
        (Nothing, Nothing) -> undefined
