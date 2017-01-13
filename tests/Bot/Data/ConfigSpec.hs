{-# LANGUAGE PatternGuards, OverloadedStrings #-}
module Bot.Data.ConfigSpec where

import qualified Data.ByteString.Lazy as LBS
import Test.Hspec
import Data.Aeson
import Data.Yaml.Config
import System.IO.Error

import Bot.Types
import Bot.Data.Config


main :: IO ()
main = hspec spec


exampleConfig :: Config
exampleConfig = Config "irc.freenode.net"
                       "nai-bot"
                       [ "#homez"
                       , "##zippy-chan"]
                       "wak3y"
                       "lfmk3y"

spec :: Spec
spec = do
  describe "Config file parsing tests:" configFromJSONTests
  describe "Getting config tests:" getConfigTests


configFromJSONTests :: Spec
configFromJSONTests = do
 describe "Testing the Config fromJSON instance:" $ do
   it "Creates a proper Config from JSON data" $ do
     jsonData  <- LBS.readFile "tests/Bot/Data/example_config.json"
     (decode' jsonData) `shouldBe` Just exampleConfig
   it "Creates a proper Config from YAML data" $ do
     yamlConfig <- loadYamlSettings ["tests/Bot/Data/example_config.yaml"]
                                    []
                                    useEnv
     yamlConfig `shouldBe` exampleConfig
   it "parseJSON instance returns a Nothing from invalid config" $ do
     shouldBe (decode' "[{\"name\":\"Joe\",\"age\":12}]" :: Maybe Config)
              Nothing

getConfigTests :: Spec
getConfigTests = do
  describe "Doesn't run without a config file but doesn't crash either: " $ do
    it "getConfig falls back to config.yaml file" $ do
      config <- (getConfig "does_not_exist.yaml")
      config `shouldBe` exampleConfig
    it "getConfig throws an IO exception from userError from invalid yaml" $ do
      shouldThrow (getConfig "tests/Bot/Data/invalid_config.yaml")
                  isUserError
