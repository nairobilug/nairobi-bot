{-# LANGUAGE Arrows #-}
module Bot.Meetup
( meetupBot
) where

import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding ((.), id)   -- we use (.) and id from `Control.Category`
import Control.Auto.Effects (arrMB)
import Data.Time.Clock

import Bot.Types



-- To do.
meetupBot :: MonadIO a => RoomBot a
meetupBot = proc (InMessage _ msg _ _) -> do

  blipPost <- emitJusts timeToPostOnIRC -< msg

  result <- arrMB (liftIO . postOnIRC) -< blipPost

  id -< (: []) <$> result

  where
    -- The string doesn't matter.
    timeToPostOnIRC :: Message -> Maybe Message
    timeToPostOnIRC _ = undefined

    postOnIRC :: String -- We don't really want this string.
              -> IO Message
    postOnIRC _ = do -- return "We have a meetup on the first Saturday of the month."
      utcTime <- getCurrentTime
      undefined
