{-# LANGUAGE RecordWildCards #-}
module BotSpec where

import Bot
import Bot.Types
import Test.Hspec
import Network.SimpleIRC.Core
import Control.Auto
import Prelude hiding ((.))
import Data.Time.Clock
import Data.Map.Lazy as M

exampleConfigValue :: Config
exampleConfigValue =
  (Config "irc.freenode.net" "nairobi-bot" ["#chan1",  "##chan2"] "" "")


matchTrigger :: String -> Maybe [String]
matchTrigger x = if x == "@trigger"
                  then (Just ["triggered"])
                  else Nothing

-- exampleRoomBot :: Auto m InMessage (Blip [Messages])
exampleRoomBot :: RoomBot m
exampleRoomBot = emitJusts (matchTrigger . message)

main :: IO ()
main = hspec spec

spec :: Spec
spec =  describe "Bot tests: " serverConfigTests

serverConfigTests :: Spec
serverConfigTests = do
  describe "Generates meaningful IrcConfig from Config" $ do
    it "`Bot.ircServer` returns expected IrcConfig value" $
      let ircConfigs = (genIrcConfig exampleConfigValue)
      in do (cAddr ircConfigs)     `shouldBe` (server exampleConfigValue)
            (cNick ircConfigs)     `shouldBe` (name exampleConfigValue)
            (cChannels ircConfigs) `shouldBe` (channels exampleConfigValue)



perRoomTests :: Spec
perRoomTests = do
  describe "perRoom correctly converts a RoomBot into a ChatBot" $ do
    it "Creates a Channel Message map" $
      pendingWith "can't remember what I was to put here"
  describe "Returns a proper OutMessage to the Channel" $ do
    it "Has an empty list message when the trigger is wrong" $
      let exampleChatBot  = perRoom exampleRoomBot
      in do
          now <- getCurrentTime
          shouldBe [(OutMessages (M.singleton "#slug" ["triggered"]))]
                   (streamAuto' exampleChatBot [InMessage "urbanslug" "@not-trigger" "#slug" now])
    it "Has an non empty list message when the trigger is correct" $
      let exampleChatBot  = perRoom exampleRoomBot
      in do
          now <- getCurrentTime
          shouldBe [(OutMessages (M.singleton "#slug" []))]
                   (streamAuto' exampleChatBot [InMessage "urbanslug" "@trigger" "#slug" now])

-- to do, quickcheck perRoom
