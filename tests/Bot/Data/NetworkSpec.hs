{-# LANGUAGE OverloadedStrings #-}
module Bot.Data.NetworkSpec where


import Test.QuickCheck
import Test.QuickCheck as QC
import Test.Hspec

import Network.Wreq
import Control.Lens
import Bot.Data.Network
import qualified Data.ByteString.Lazy as LB
-- import Data.ByteString.Internal as IB
-- import Network.HTTP.Types.Header
import Data.ByteString.Char8 as Char8
import Network.HTTP.Client.Internal as HC hiding (responseBody)
import Network.HTTP.Types.Version
import Network.HTTP.Types.Status
import Bot.Types
import Data.Word (Word8)


newtype TestResponse = TestResponse
  {getTestResponse :: (Response LB.ByteString)} deriving (Eq, Show)

instance Arbitrary TestResponse where
  arbitrary = do
    let html  = "text/html; charset=utf-8"
        html1 = "text/html"
        html2  = "text/html;charset=utf-8"
        gif   = "image/gif"
        jpeg  = "image/jpeg"
        video = "video/mp4"
        toBS  = Char8.pack . show
    -- :: Gen Header
    respBody <- arbitrary :: Gen [Word8]
    contentType' <- QC.elements $ (,) contentTypeResponseHeader
                    <$> [html, html1, html2, gif , jpeg, video]
    intLength <- arbitrary :: Gen Integer
    let contentLength' = (contentLengthResponseHeader, (toBS intLength))
        lazyResponseBody = LB.pack respBody
    return $ TestResponse $ HC.Response (Status 200 "OK")
                                        (HttpVersion 1 1)
                                        [contentType' , contentLength']
                                        lazyResponseBody
                                        mempty
                                        (ResponseClose (return ()))


oneMBLazyByteString :: LB.ByteString
oneMBLazyByteString = LB.replicate maxResponseSize 0

exampleResponse :: HC.Response LB.ByteString
exampleResponse =  HC.Response (Status 200 "OK")
                               (HttpVersion 1 1)
                               []
                               oneMBLazyByteString
                               mempty
                               (ResponseClose (return ()))

exampleTestResponse :: TestResponse
exampleTestResponse = TestResponse exampleResponse

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Network tests: " networkProperties

propTruncateResponse :: TestResponse -> Bool
propTruncateResponse (TestResponse resp) =
  let truncatedResponse = truncateResponseBody resp
  in (LB.length $ truncatedResponse ^. responseBody) <=
     (LB.length $ exampleResponse ^. responseBody)

propParseResponse :: TestResponse -> Bool
propParseResponse (TestResponse resp) =
   let parsedResp = parseResponse resp
       len = resp ^. responseHeader contentLengthResponseHeader
       typ = resp ^. responseHeader contentTypeResponseHeader
       truncatedBod = LB.toStrict  $ truncateResponseBody resp ^. responseBody
   in parsedResp == BotResponse truncatedBod typ len

networkProperties :: Spec
networkProperties =
  describe "Network property tests: " $ do
    it "Truncates the response to 1 MegaByte: " $ do
      property $ forAll (arbitrary :: Gen TestResponse) propTruncateResponse
    it "Reliably parses the Response into a BotResponse" $ do
      property $ forAll (arbitrary :: Gen TestResponse) propParseResponse
