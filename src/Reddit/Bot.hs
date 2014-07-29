module Reddit.Bot where

import Reddit.API

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)

newtype Bot = Bot { unBot :: Reddit () }

instance Monoid Bot where
  mappend (Bot b1) (Bot b2) = Bot (b1 >> b2)
  mempty = Bot (return ())

runBots :: [Bot] -> Text -> Text -> IO (Either (APIError RedditError) ())
runBots bs user pass = runReddit user pass $ void $ unBot $ mconcat bs

every :: MonadIO m => Int -> RedditT m a -> RedditT m ()
every n act = forever $ void act >> wait n

-- | Wait @n@ seconds. Essentially @threadDelay@ in the @Reddit@ type using seconds
--   instead of microseconds.
wait :: MonadIO m => Int -> RedditT m ()
wait = liftIO . threadDelay . (* 1000000)
