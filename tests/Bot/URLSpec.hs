{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Bot.URLSpec where

import Bot.URL
import Test.Hspec
import Test.QuickCheck
import Bot.Types
import Data.ByteString hiding (any, concat)
import Data.Time.Clock
import Control.Auto
import Control.Auto.Blip.Internal
import qualified Data.List as L


newtype TestBotResponse = TestBotResponse
  {getTestBotResponse :: (BotResponse ByteString)} deriving (Eq, Show)
newtype TestBlip = TestBlip {getBlip :: Blip [String]} deriving (Show)

instance Eq TestBlip where
  (TestBlip (Blip f)) == (TestBlip (Blip s)) = f == s
  (TestBlip NoBlip) == (TestBlip NoBlip) = True
  (TestBlip (Blip _)) ==  (TestBlip NoBlip) = False
  (TestBlip NoBlip) == (TestBlip (Blip _)) = False

instance Arbitrary TestBotResponse where
  arbitrary =
    let html  = "text/html; charset=utf-8"
        html1 = "text/html"
        gif   = "image/gif"
        jpeg  = "image/jpeg"
        video = "video/mp4"
    in do contentType' <- elements [html, html1, gif , jpeg, video]
          return $ TestBotResponse $ BotResponse "" contentType'  ""

propHandleResponse :: TestBotResponse  -> Bool
propHandleResponse resp =
  let texty = handleResponse $ getTestBotResponse resp
      contentType' = contentType $ getTestBotResponse resp
  in if (any (== contentType') ["text/html; charset=utf-8"])
        then (L.isInfixOf "Title: " texty) == True
        else (L.isInfixOf "Content-Type: " texty) == True

main :: IO ()
main = hspec spec

spec :: Spec
spec =  describe "URL tests: " $ do
        urlTests
        botTests

botTests :: Spec
botTests = do
  describe "Bot reacts to URLs" $ do
    it "Is NoBlips non URLs" $ do
      now <- getCurrentTime
      f <- streamAuto (emitJusts (const Nothing)) (["a"] :: [String])
      s <- streamAuto (urlBot :: RoomBot IO)
                      [InMessage "slug" "Rando message" "#slug-chan" now]
      (fmap TestBlip f) `shouldBe` (fmap TestBlip s)
    it "Non empty for URLs" $ do
       pendingWith "Can't mock a network call."


urlTests :: Spec
urlTests = do
  describe "Extracts URL details given a URL" $ do
    it "Has Title for type utf8 html and Content-Type for anything else" $
      property $ forAll (arbitrary :: Gen TestBotResponse) propHandleResponse
