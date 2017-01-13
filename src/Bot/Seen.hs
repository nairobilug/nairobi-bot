{-# LANGUAGE Arrows, InstanceSigs #-}
module Bot.Seen where

import Control.Auto
import Data.Map as M
import Data.Time.Clock
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Monad.IO.Class
import Bot.Types



seenBot :: MonadIO m => RoomBot m
seenBot = proc (InMessage nick msg _ time) -> do

    now <- effect (liftIO getCurrentTime) -< ()

    seens  <- trackSeens -< (nick, time)

    queryB <- queryBlips -< msg

    let respond :: Nick -> [Message]
        respond qry = case M.lookup qry seens of
                        Just t  -> [qry ++ " last seen "++ breakTime (diffUTCTime now t) ++ "ago."]
                        Nothing -> ["No record of " ++ qry ++ "."]

    id -< respond <$> queryB
  where
    trackSeens :: Monad m => Auto m (Nick, UTCTime) (Map Nick UTCTime)
    trackSeens = accum (\mp (nick, time) -> M.insert nick time mp) M.empty

    queryBlips :: Auto m Message (Blip Nick)
    queryBlips = emitJusts (getRequest . words)
      where
        getRequest ("@seen":nick:_) = Just nick
        getRequest _                = Nothing


breakTime :: NominalDiffTime -> String
breakTime diffTime =
  let rounded = round diffTime
      (m',s) = quotRem rounded 60
      (h', m) = quotRem m' 60
      (d, h) = quotRem h' 24
      sh = show
  in sh d ++ " days " ++ sh h ++ " hours " ++ sh m ++ " minutes and " ++ sh s ++ " seconds "
