{-# LANGUAGE OverloadedStrings #-}
module Bot.Wolfram where

import Data.String
import System.Environment
-- import Network.Wreq.Types
import Network.Wreq
import Control.Lens
import Data.Text hiding (length, head, map)
import qualified Data.ByteString.Char8 as Char8
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
-- import Data.Text.Lazy.Encoding (decodeUtf8)
-- import Text.Taggy
-- import qualified Data.Text.Lazy as LT
-- import qualified Data.HashMap.Strict as HM
import Text.HTML.TagSoup
-- import Text.HTML.TagSoup.Match
-- let opts = defaults & header (fromString "Accept") .~ [(pack "text/xml")] & param (T.pack "input") .~ [(T.pack "cat")] & param (T.pack "appid") .~ [(T.pack "<appId>")]
-- r <- getWith opts "http://api.wolframalpha.com/v2/query?"
-- r ^. responseBody

appID :: IO Text
appID = fmap pack $ getEnv "WAappID"

query :: Text
query = "5*5"

opts :: Options
opts = defaults & header (fromString "Accept") .~ [(Char8.pack "text/xml")] & param (pack "input") .~ [query] & param (pack "appid") .~ [(pack "6TJA3R-PQ9AP7W8UP")]


sendQuery :: IO L.ByteString
sendQuery = do
  r <- getWith opts "http://api.wolframalpha.com/v2/query?" -- ^:: IO (Response L.ByteString)
  return $ r ^. responseBody
  
tagList :: IO [Tag L.ByteString]
tagList = fmap parseTags sendQuery

tagStrBool :: Tag L.ByteString -> Bool
tagStrBool (TagOpen str _)
  |str == "?xml" = True
  | otherwise = False
tagStrBool _ = False

matchAttr :: [Attribute L.ByteString] -> Bool
matchAttr [] = False
matchAttr (x:xs)
  | x == ("id","Result") = True
  | otherwise = matchAttr xs

matchPlainText :: [Attribute L.ByteString] -> Bool
matchPlainText [] = True
matchPlainText _ = False

--  r <- tagList
-- let biss = getTagContent (fromString "pod") matchAttr r
-- Prelude.map maybeTagText $ getTagContent (fromString "plaintext") matchPlainText biss

safeHead :: [Maybe L.ByteString] -> Maybe L.ByteString
safeHead [] = Nothing
safeHead (x:_) = x
