{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Bot.EchoSpec where

import Prelude hiding ((.))
import Bot.Echo
import Test.Hspec
import Test.QuickCheck
import Bot.Types
import Data.Time.Clock
import Control.Auto
import System.IO.Unsafe
import Control.Auto.Blip.Internal


newtype TestBlip = TestBlip {getBlip :: Blip [String]} deriving (Show)

instance Eq TestBlip where
  (TestBlip (Blip f)) == (TestBlip (Blip s)) = f == s
  (TestBlip NoBlip) == (TestBlip NoBlip) = True
  (TestBlip (Blip _)) ==  (TestBlip NoBlip) = False
  (TestBlip NoBlip) == (TestBlip (Blip _)) = False

instance Arbitrary TestBlip where
  arbitrary = do
    str <- arbitrary :: Gen String
    return $ TestBlip $ Blip [str]

main :: IO ()
main = hspec spec

spec :: Spec
spec =  describe "URL tests: " $ do echoBotTests
                                    echoBotProperties
-- to do fix
propEcho :: String -> Bool
propEcho  str =
  unsafePerformIO fu
  where fu =
         let echoStr = "@echo " ++ str
             now = unsafePerformIO getCurrentTime
         in do
           f <- streamAuto
                  (emitJusts (const $ Just [str]))
                  (["a"] :: [String])
           s <- streamAuto echoBot [InMessage "slug" echoStr "#slug-chan" now]
           return $ (fmap TestBlip f) == (fmap TestBlip s)

echoBotProperties :: Spec
echoBotProperties = do
  describe "Echo property tests" $ do
    it "Echobot just echoes what it receives but escapes unsafe chars" $ do
      --property $ forAll (arbitrary :: Gen String) propEcho
      pendingWith "Fix propEcho"


echoBotTests :: Spec
echoBotTests = do
  describe "Correctly does echo-ing" $ do
    it "Doesn't echo without the trigger @echo" $ do
      now <- getCurrentTime
      f <- streamAuto (emitJusts (const Nothing)) (["a"] :: [String])
      s <- streamAuto (echoBot :: RoomBot IO)
                      [InMessage "slug" "Rando message" "#slug-chan" now]
      (fmap TestBlip f) `shouldBe` (fmap TestBlip s)
    it "echoes for the trigger @echo" $ do
      now <- getCurrentTime
      f <- streamAuto (emitJusts (const $ Just ["Nyan"])) (["a"] :: [String])
      s <- streamAuto (echoBot :: RoomBot IO) [InMessage "slug" "@echo Nyan" "#slug-chan" now]
      (fmap TestBlip f) `shouldBe` (fmap TestBlip s)
