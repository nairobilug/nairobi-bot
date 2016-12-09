{-|
Module      : Bot.Meetup
Description : Notify users on IRC about nairobilug meetups.
Copyright   : (c) 2015, Njagi Mwaniki 
License     : BSD3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

We meet up every first Saturday of the month.
The bot posts a notification on IRC:
10 days, 7 days, 5 days, 3 days, 2 days and 1 day 
towards the first Satruday of every month.
-}

{-# LANGUAGE Arrows #-}
module Bot.Meetup
( meetupBot
) where

import Control.Auto
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude hiding           ((.), id)   -- we use (.) and id from `Control.Category`
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
