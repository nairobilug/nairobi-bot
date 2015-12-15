{-|
Module      : Bot.URL
Description : Fetch the page title and description of a webpage given the url.
Copyright   : (c) 2015, Njagi Mwaniki 
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.URL
( urlBot
) where

-- Bot itself.
import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto.Effects (arrMB)

import Bot.Types

-- Heavy lifting
import Bot.Network (getWebPage)
-- import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.String as S
import qualified Data.List as L
import Text.HTML.TagSoup -- (parseTags, maybeTagText, Tag, Attribute, (~==))
import Text.HTML.TagSoup.Match


urlBot :: MonadIO a => RoomBot a
urlBot = proc (InMessage _ msg _ _) -> do

  blipMsg <- emitJusts containsUrl -< msg

  result <- arrMB (liftIO . getTitle) -< blipMsg

  id -< (: []) <$> result

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
                   || "://" `L.isInfixOf` str
    
    getTitle :: Message -> IO Message
    getTitle msg = fmap (("Title: "++) . (L.intercalate "\nTitle: "))  $ fmap (map T.unpack) $ urlList $ S.words msg

    urlList :: [String] -> IO [T.Text]
    urlList [] = return []
    urlList (x:xs) =
      if isStringUrl x
         then do
           title' <- extractTitle x
           let title = if title' == "Notfound.org"
                          then "The webpage could not be fetched due to a HTTP exception. Not our fault."
                          else title'
           fmap (title :) (urlList xs)
         else urlList xs




extractTitle :: String -- | URL
             -> IO T.Text -- | Title
extractTitle url = do
  eitherWebPage <- getWebPage url  -- webpage :: L.ByteString
  case eitherWebPage of
    Right webpage -> do
      let matchAttr = (\x -> if x == []
                                then True
                                else False)
          parsed =  parseTags webpage
          content = getTagContent (S.fromString "title") matchAttr parsed
          (maybeResult:_) = fmap maybeTagText content
      case maybeResult of
        Just result ->  return $ TE.decodeUtf8 $ LB.toStrict result -- To do: handle web pages that don't use UTF8
        Nothing     -> return "Could not fetch a title for that URL. \
                              \If it's not a problem with your URL maybe the \
                              \page uses a different encoding from UTF8. If not, \
                              \report a bug at: https://github.com/nairobilug/nairobi-bot/issues"
    Left ex -> return $ T.pack $ "Fetching page title failed due to " ++ ex
