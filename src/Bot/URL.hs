{-# LANGUAGE OverloadedStrings, RecordWildCards, Arrows #-}
module Bot.URL where

import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto.Effects (arrMB)
import Control.Monad
import Bot.Types
import Bot.Data.Network
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as Char8 hiding (foldr, map, any)
import qualified Data.String as S
import qualified Data.List as L
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Printf
import Data.Text.Encoding


urlBot :: MonadIO a => RoomBot a
urlBot = proc (InMessage _ msg _ _) -> do

  blipMsg <- emitJusts containsUrl -< msg

  result <- arrMB (liftIO . getTitle) -< blipMsg

  id -< result

  where
    -- Does the message contain a url one or more?
    -- If so give us back the message. If not, ignore it.
    containsUrl :: Message -> Maybe Message
    containsUrl msg =
      if foldr (||) False $ map isStringUrl $ S.words msg
         then Just msg
         else Nothing

    isStringUrl :: String -> Bool
    isStringUrl str =
      "http://" `L.isInfixOf` str || "https://" `L.isInfixOf` str

    getTitle :: Message -> IO [Message]
    getTitle msg = urlList $ S.words msg

    urlList :: [URL] -> IO [String]
    urlList []  = return []
    urlList lst = mapM pageDetails $ L.filter isStringUrl lst

extractTitle :: ByteString -> String
extractTitle html =
  let tagList = parseTagsOptions parseOptions{optTagPosition = True} $ decodeUtf8 html
      title = "title" :: T.Text
      hasHeadTag =
        foldr (||) False  $ Prelude.map (isTagOpenName title) tagList
      titleTag =
        T.unpack $ renderTagsOptions renderOptions{optEscape = id} $ getTagContent title (const True) tagList
  in if hasHeadTag
        then titleTag
        else "Page title not found."

handleResponse :: BotResponse ByteString -> String
handleResponse BotResponse{..} =
  if isHTML && isUtf8
     then htmlText (T.strip $ T.pack $ extractTitle body)
                   (convertBytes contentLength)
     else anyOther contentType (convertBytes contentLength)
  where lowerCaseContentType = T.toLower $ decodeUtf8 contentType
        isHTML = T.isInfixOf "text/html" lowerCaseContentType
        isUtf8 = T.isInfixOf "utf-8" lowerCaseContentType || T.isInfixOf "utf8" lowerCaseContentType
        htmlText title s = printf "Size: [%s] Title: [%s]" s title
        anyOther typ s   = printf "Content-Type: [%s] Size: [%s]" (Char8.unpack typ) s

pageDetails :: URL -> IO String
pageDetails url = do
  eitherResponse <- safeGet url
  case eitherResponse of
    Right response -> return $ handleResponse $ parseResponseTruncated response
    Left  _        -> return "Page title not found."
     -- return $ fmap T.unpack  $ [(T.pack $ "Fetching URL data failed due to " ++ show err)]
