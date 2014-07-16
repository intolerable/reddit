module Reddit.Bot where

import Reddit.API

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
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

-- | Rate limit a @Reddit@ action, waiting for 60 seconds if the rate limit is reached.
--
-- > rateLimit = rateLimitWith (wait 60)
rateLimit :: MonadIO m => RedditT m a -> RedditT m a
rateLimit = rateLimitWith (wait 60)

-- | Rate limit an action using a specified action. If the rate limit is reached when
--   running the second action, the first action is executed, then the first action is run
--   again.
rateLimitWith :: MonadIO m => RedditT m () -> RedditT m a -> RedditT m a
rateLimitWith onLimit act = do
  res <- nest act
  case res of
    Left (APIError (RateLimitError msg)) -> do
      RedditT $ liftIO $ print msg
      onLimit
      rateLimitWith onLimit act
    Left x -> RedditT . EitherT . return $ Left x
    Right x -> RedditT $ return x

-- | Wait @n@ seconds. Essentially @threadDelay@ in the @Reddit@ type using seconds
--   instead of microseconds.
wait :: MonadIO m => Int -> RedditT m ()
wait = liftIO . threadDelay . (* 1000000)
