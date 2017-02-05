{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Bot.Types where

import Control.Auto.Blip
import Control.Auto
import qualified Data.Map as M
import Data.Time
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text hiding (length, null)
import qualified Data.Vector as V
import Data.Serialize
import System.Random
import System.IO.Unsafe

type Nick     = String
type Channel  = String
type Message  = String
type URL      = String
type Username = String
type Query    = String

instance Serialize UTCTime where
  get = read <$> get -- to do: refactor this
  put = put . show

instance Serialize Day where
  get = ModifiedJulianDay <$> get
  put = put . toModifiedJulianDay

data NowPlaying = NowPlaying { song :: Text
                             , artist :: Text
                             , album :: Text
                             } deriving Show

data Definition = Definition { abstractText :: Text
                             , getRelatedTopics :: Text
                             } deriving Show

data InMessage = InMessage { nick    :: Nick
                           , message :: Message
                           , channel :: Channel
                           , time    :: UTCTime
                           } deriving Show

data BotResponse a = BotResponse { body :: a
                                 , contentType :: a
                                 , contentLength :: a} deriving (Show, Eq)

newtype OutMessages = OutMessages (M.Map Channel [Message]) deriving (Show, Eq)

instance Monoid OutMessages where
    mempty  = OutMessages M.empty
    mappend (OutMessages m1) (OutMessages m2)
            = OutMessages (M.unionWith (++) m1 m2)

type ChatBot m = Auto m InMessage OutMessages

type RoomBot m = Auto m InMessage (Blip [Message])

instance FromJSON NowPlaying where
--  parseJSON :: FromJSON NowPlaying => Value -> Parser NowPlaying
  parseJSON (Object o) = do
    recentTracksObj <- o .: "recenttracks"
    case recentTracksObj of
      Object a -> do
        trackObj <- a .: "track"
        case trackObj of
          Array b ->
            case V.toList b of
              [] -> fail "Array is empty"
              (x:_) -> case x of
                              Object b' -> do
                                song' <- b' .: "name"
                                artistObj <- b' .: "artist"
                                artist' <- case artistObj of
                                             Object c -> c .: "#text"
                                             _ -> fail "FU"
                                albumObj <- b' .: "album"
                                case albumObj of
                                  Object d -> fmap (NowPlaying song' artist') $ d .: "#text"
                                  _ -> fail "idc"
                              _ -> fail "Idk"
          _ -> fail "idk"
      _ -> fail "Bad JSON"
  parseJSON _ = fail "Really bad JSON"

-- The first arg to forM is a list of Objects
-- The second is one that takes a list of Objects and extracts Text values from it.
-- topicsArray :: Vector Value. You can get a [Value] from that using Data.Vector.toList
-- Objects .: Object -> Text
instance FromJSON Definition where
  parseJSON (Object v) = do
    abText <- v .: "AbstractText"
    topics <- v .: "RelatedTopics"
    case topics of
      Array topicsArray ->
        fmap (Definition abText) $ case V.toList topicsArray of
                     [] -> fail "Empty Related topics array"
                     (x:_) -> (\elem' -> case elem' of
                               Object o -> o .: "Text"
                               _ -> fail "Related topics isn't a JSON object."
                             ) x
      _ -> fail "No array in related topics."
  parseJSON _ = fail "No JSON object in JSON file."

newtype GIF = GIF {gifURL :: URL} deriving (Show)

instance FromJSON GIF where
  parseJSON (Object d) = do
    dataObj <- d .: "data"
    case dataObj of
      Array a ->
        case V.toList a of
          [] -> fail "Data Array is empty"
          lst ->
            case lst!!(unsafePerformIO $ randomRIO (0, (length lst - 1))) of
              Object b -> do
                imagesObj <- b .: "images"
                case imagesObj of
                  Object c -> do
                    originalObj <- c .: "original"
                    case originalObj of
                      Object e -> fmap GIF $ e .: "url"
                      _ -> fail " origial value is empty"
                  _ -> fail "original is empty"
              _ -> fail "images is empty"
      _ -> fail "data is empty"
  parseJSON invalidJSON = typeMismatch "Did not find JSON Object in config but found: " invalidJSON

data Config = Config { server       :: String
                     , name         :: String
                     , channels     :: [String]
                     , wolframAlpha :: String
                     , lastFm       :: String
                     } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                           v .: "server" <*>
                           v .: "name" <*>
                           v .: "channels" <*>
                           v .: "wolframAlpha" <*>
                           v .: "lastFm"
  parseJSON invalidJSON =
    typeMismatch "Did not find JSON Object in config but found: " invalidJSON
