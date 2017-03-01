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
    isStringUrl str = "://" `L.isInfixOf` str

    getTitle :: Message -> IO [Message]
    getTitle msg = (fmap . fmap) T.unpack $ urlList $ S.words msg

    urlList :: [URL] -> IO [T.Text]
    urlList []  = return []
    urlList lst = fmap join $ mapM pageDetails $ L.filter isStringUrl lst

extractTitle :: ByteString -> String
extractTitle html =
  let tagList = parseTags html
      title = "title" :: ByteString
      hasHeadTag =
        foldr (||) False  $ Prelude.map (isTagOpenName title) tagList
      titleTag =
        Char8.unpack $ renderTags $ getTagContent title (const True) tagList
  in if hasHeadTag
        then titleTag
        else "Page title not found."

handleResponse :: BotResponse ByteString -> [T.Text]
handleResponse BotResponse{..} =
  if isHTML
     then htmlText (extractTitle body) (convertBytes contentLength)
     else anyOther contentType (convertBytes contentLength)
  where isHTML = Char8.isInfixOf "text/html" contentType
        htmlText title s = [T.pack $ printf "Size: [%s] Title: [ %s ]" s (T.strip $ T.pack title)]
        anyOther typ s   = [T.pack $ printf "Content-Type: [%s] Size: [%s]"
                                            (Char8.unpack typ) s]

pageDetails :: URL -> IO [T.Text]
pageDetails url = do
  eitherResponse <- safeGet url
  case eitherResponse of
    Right response -> return $ handleResponse $ parseResponseTruncated response
    Left  err ->
     return $ [(T.pack $ "Fetching URL data failed due to " ++ show err)]
