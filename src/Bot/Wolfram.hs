{-|
Module      : Bot.Wolfram
Description : Query wolfram alpha <http://www.wolframalpha.com/>
Copyright   : (c) 2015, Njagi Mwaniki 
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
module Bot.Wolfram (waBot) where

-- Bot itself.
import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto.Effects (arrMB)



-- Heavy lifting parsing stuff.
import qualified Data.String as String
import Data.Text hiding (length, head, map)
import qualified Data.ByteString.Lazy as L
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.Text.Encoding (decodeUtf8)

import Bot.Types
import Bot.Network (sendWaQuery)
import Data.Bot.Config (getConfig)


waBot :: MonadIO a => RoomBot a
waBot = proc (InMessage _ msg _ _) -> do

  blipQuery <- emitJusts getQuery -< msg

  result <- arrMB (liftIO . getWolfram) -< blipQuery

  id -< (: []) <$> result

  where
    getQuery :: Message -> Maybe Message
    getQuery query = 
      case String.words query of
        ("@wa": q) -> Just $ String.unwords q
        _          -> Nothing

    getWolfram :: Message -> IO Message
    getWolfram query = fmap unpack $ wolfram query

    wolfram :: String  -- | Query
            -> IO Text -- | IO result
    wolfram query = do
      lst <- getTagList $ pack query

      let biss = getTagContent (String.fromString "pod") matchAttr lst
          (maybeResult:_) = Prelude.map maybeTagText $ getTagContent (String.fromString "plaintext") matchPlainText biss

      case fmap (decodeUtf8 . L.toStrict) maybeResult of
        Just result -> return result
        Nothing     -> return "No result from wolfram alpha.\
                               \ If it's not a bad query or API key\
                               \ please report a bug at\
                               \ https://github.com/urbanslug/nairobi-bot/issues"

{- 
   Parsing and the heavy lifting.
-}

appID :: IO Text
appID = do 
  conf' <- getConfig
  let conf = case conf' of
               Just c -> c
               Nothing -> Config "" "" [] "" "" -- Fail silently because the configs are not a must here.
  return $ pack $ wolframAlpha conf


getTagList :: Text -> IO [Tag L.ByteString]
getTagList query = do
  key' <- appID
  key <- case key' of
             "" -> fail "You haven't set your wolfram alpha API key. Fix your config.yaml."
             k -> return k
  fmap parseTags (sendWaQuery query key)

matchAttr :: [Attribute L.ByteString] -> Bool
matchAttr [] = False
matchAttr (x:xs)
  | x == ("title", "Result") = True
  | otherwise = matchAttr xs

matchPlainText :: [Attribute L.ByteString] -> Bool
matchPlainText [] = True
matchPlainText _ = False



