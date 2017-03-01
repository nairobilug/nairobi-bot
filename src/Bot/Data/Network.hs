{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Bot.Data.Network where

import Network.Wreq
import Data.Int (Int64)
import Control.Lens
import Control.Exception.Base (try)
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Internal as IB
import Data.ByteString.Char8 as Char8
import qualified Data.String as S
import Bot.Types
import Data.Text as T
import Numeric
import Network.HTTP.Client (HttpException(..))

-- Use a max 100 MB response
maxResponseSize :: Int64
maxResponseSize = 100*1024^2

contentTypeResponseHeader :: HeaderName
contentTypeResponseHeader = "Content-Type"

contentLengthResponseHeader :: HeaderName
contentLengthResponseHeader = "Content-Length"

-- For Wolfram Alpha.
opts :: T.Text -> T.Text -> Options
opts id' query = defaults
  & header (S.fromString "Accept") .~ [(Char8.pack "text/xml")]
  & param (T.pack "input") .~ [query] & param (T.pack "appid") .~ [id']

convertBytes :: ByteString -> String
convertBytes "" = "unknown"
convertBytes size =
  let kb       = 1024 :: Double
      mb        = 1024^2 :: Double
      size'     = read $ Char8.unpack size
      div' den = (Prelude.take 4 $ showFFloat Nothing (size'/den) "")
  in if size' <= mb
       then div' kb ++ " KB"
       else div' mb ++ " MB"

-- Truncates to 1MB
parseResponseTruncated :: Response LB.ByteString -> BotResponse ByteString
parseResponseTruncated resp =
  let parsedResp = parseResponse resp
      type'   = LB.toStrict $ contentType parsedResp
      length' = LB.toStrict $ contentLength parsedResp
      bod     = LB.toStrict $ LB.take maxResponseSize $ body parsedResp
  in BotResponse bod type' length'

parseResponse :: Response LB.ByteString -> BotResponse LB.ByteString
parseResponse resp =
  let type'   = LB.fromStrict $ resp ^. responseHeader contentTypeResponseHeader
      length' = LB.fromStrict $ resp ^. responseHeader contentLengthResponseHeader
      truncatedBody = resp ^. responseBody
  in BotResponse truncatedBody type' length'

safeGet :: URL ->  IO (Either T.Text (Response LB.ByteString))
safeGet url = do
  eitherResponse <- try (get url)
                    :: IO (Either HttpException (Response LB.ByteString))
  case eitherResponse of
    Right response -> return $ Right response
    Left ex        -> return $ Left $ T.pack $ "Error: " ++  show ex


safeGetWith :: URL -> T.Text -> T.Text -> IO (Either T.Text (Response LB.ByteString))
safeGetWith url query key = do
  eitherResponse <- try $ getWith (opts key query) url
                    :: IO (Either HttpException (Response LB.ByteString))
  case eitherResponse of
    Right response -> return $ Right response
    Left  ex       -> return $ Left $ T.pack $ "Error: " ++  show ex
