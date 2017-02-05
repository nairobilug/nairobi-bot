module Bot.TypesSpec where

import Test.Hspec
import Data.Aeson
import Bot.Types
import qualified Data.ByteString.Lazy as LBS
import Data.List


spec :: Spec
spec = describe "Type tests: " parseJSONTests

parseJSONTests :: Spec
parseJSONTests = do
  describe "NowPlaying parseJSON instance makes sense." $ do
    it "Now Playing" $
     pending
  describe "Definition parseJSON instance makes sense." $ do
    it "Definition" $
       pending
  describe "GIF parseJSON instance makes sense." $ do
    it "Successfully decodes GIF JSON" $ do
      jsonGIF <- LBS.readFile "tests/Bot/GIF.json"
      let (Just gif) = decode jsonGIF
      isInfixOf "://" (gifURL gif) `shouldBe` True
