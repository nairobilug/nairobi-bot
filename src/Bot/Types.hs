{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
These types are specific to chatbot and are not irc related.
-}

module Bot.Types where

import Control.Auto.Blip
import Control.Auto
-- -- import Data.Monoid
import qualified Data.Map as M
import Data.Time
import Data.Serialize
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`

import Data.Aeson
import Data.Text

import qualified Data.Vector as V

type Nick    = String
type Channel = String
type Message = String

type URL = String
type Username = String

data InMessage = InMessage { _inMessageNick   :: Nick
                           , _inMessageBody   :: Message
                           , _inMessageSource :: Channel
                           , _inMessageTime   :: UTCTime
                           } deriving Show

newtype OutMessages = OutMessages (M.Map Channel [Message])
                    deriving Show

data NowPlaying =
  NowPlaying { song :: Text
             , artist :: Text
             , album :: Text
             } deriving Show

instance Monoid OutMessages where
    mempty  = OutMessages M.empty
    mappend (OutMessages m1) (OutMessages m2)
            = OutMessages (M.unionWith (++) m1 m2)


type ChatBot m = Auto m InMessage OutMessages

type RoomBot m = Auto m InMessage (Blip [Message])

instance Serialize UTCTime where
    get = read <$> get      -- haha don't do this in real life.
    put = put . show

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay


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
        {- case trackObj of
             Object b -> do
               song' <- b .: "name"
               artistObj <- b .: "artist"
               artist' <- case artistObj of
                            Object c -> c .: "#text"
                            _ -> fail "FU"
               albumObj <- b .: "album"
               case albumObj of
                 Object d -> fmap (NowPlaying song' artist') $ d .: "#text"
                 _ -> fail "idc"
             _ -> fail "Idk"-}
      _ -> fail "Bad JSON"
  parseJSON _ = fail "Really bad JSON"


-- newtype Definition =
--  Definition {getRelatedTopics :: Text} deriving Show

data Definition = 
  Definition { abstractText :: Text
             , getRelatedTopics :: Text
  } deriving Show


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
