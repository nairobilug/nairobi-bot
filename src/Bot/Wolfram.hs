{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.Wolfram (waBot) where

-- Bot itself.
import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto.Effects (arrMB)

-- Heavy lifting parsing stuff.
import qualified Data.String as S
import Data.Text hiding (length, head, map)
import qualified Data.ByteString.Lazy as L
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.Text.Encoding (decodeUtf8)

import Bot.Types
import Bot.Data.Network
import Bot.Data.Config (getConfig)



waBot :: MonadIO a => RoomBot a
waBot = proc (InMessage _ msg _ _) -> do

  blipQuery <- emitJusts getQuery -< msg

  result <- arrMB (liftIO . getWolfram) -< blipQuery

  id -< (: []) <$> result

  where
    getQuery :: Message -> Maybe Message
    getQuery query =
      case S.words query of
        ("@wa": query') -> Just $ S.unwords query'
        _               -> Nothing

    getWolfram :: Message -> IO Message
    getWolfram query = fmap unpack $ wolfram query

    wolfram :: Query -> IO Text
    wolfram query = do
      tagList <- getTagList $ pack query

      let errorText   = "No result from wolfram alpha."
          podContent  = getTagContent (S.fromString "pod") matchAttr tagList
          plainText   = getTagContent (S.fromString "plaintext") matchPlainText podContent
          maybeResult = case Prelude.map maybeTagText plainText  of
                          (x:_) -> x
                          [] -> Nothing

      if Prelude.foldr (||) False $ fmap (tagOpenAttrNameLit "pod" "title" (== "Result")) tagList
        then case fmap (decodeUtf8 . L.toStrict) maybeResult of
               Just result -> return result
               Nothing     -> return errorText
        else return errorText

appID :: IO Text
appID = do
  conf <- getConfig "config.yml"
  return $ pack $ wolframAlpha conf


getTagList :: Text -> IO [Tag L.ByteString]
getTagList query = do
  key' <- appID
  key <- case key' of
           "" -> fail "Set your wolfram alpha API key. Fix your config.yaml."
           k -> return k
  eitherResponse <-
    safeGetWith "http://api.wolframalpha.com/v2/query?" query key
  case eitherResponse of
    Left _     -> return []
    Right resp -> return $ parseTags $ L.fromStrict $ body $ parseResponse resp

matchAttr :: [Attribute L.ByteString] -> Bool
matchAttr [] = False
matchAttr (x:xs)
  | x == ("title", "Result") = True
  | otherwise = matchAttr xs

matchPlainText :: [Attribute L.ByteString] -> Bool
matchPlainText [] = True
matchPlainText _  = False
