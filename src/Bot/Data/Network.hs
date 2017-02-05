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
import Network.HTTP.Client (HttpException(..))

-- Use a max 1 MB response
maxResponseSize :: Int64
maxResponseSize = 1024^3

contentTypeResponseHeader :: HeaderName
contentTypeResponseHeader = "Content-Type"

contentLengthResponseHeader :: HeaderName
contentLengthResponseHeader = "Content-Length"

-- For Wolfram Alpha.
opts :: T.Text -> T.Text -> Options
opts id' query = defaults
  & header (S.fromString "Accept") .~ [(Char8.pack "text/xml")]
  & param (T.pack "input") .~ [query] & param (T.pack "appid") .~ [id']

-- What to do in case of an empty or less than expected size response?
truncateResponseBody :: Response LB.ByteString -> Response LB.ByteString
truncateResponseBody = fmap (LB.take maxResponseSize)

toMB :: ByteString -> String
toMB "" = "unknown"
toMB size =
  let kb = 1024^2 :: Double
      size' = read $ Char8.unpack size
  in (Prelude.take 4 $ show $ size'/kb)


parseResponse :: Response LB.ByteString -> BotResponse ByteString
parseResponse resp =
  let truncatedResp = truncateResponseBody resp
      type'   = resp ^. responseHeader contentTypeResponseHeader
      length' = resp ^. responseHeader contentLengthResponseHeader
      truncatedBody = LB.toStrict $ truncatedResp ^. responseBody
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
