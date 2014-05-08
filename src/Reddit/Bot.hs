module Reddit.Bot where

import Reddit.API

import Control.Concurrent
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)

newtype Bot = Bot { unBot :: Reddit () }

instance Monoid Bot where
  mappend (Bot b1) (Bot b2) = Bot (b1 >> b2)
  mempty = Bot (return ())

runBots :: [Bot] -> Text -> Text -> IO ()
runBots bs user pass = void $ runReddit user pass $ unBot $ mconcat bs

every :: Int -> Reddit a -> Reddit ()
every n act = forever $ void act >> wait n

-- | Rate limit a @Reddit@ action, waiting for 60 seconds if the rate limit is reached.
--
-- > rateLimit = rateLimitWith (wait 60)
rateLimit :: Reddit a -> Reddit a
rateLimit = rateLimitWith (wait 60) 

forkReddit :: Reddit a -> Reddit ()
forkReddit a = do
  io <- makeIO a
  void $ liftIO $ forkIO $ void io

-- | Rate limit an action using a specified action. If the rate limit is reached when 
--   running the second action, the first action is executed, then the first action is run
--   again.
rateLimitWith :: Reddit () -> Reddit a -> Reddit a
rateLimitWith onLimit act = do
  res <- nest act
  case res of 
    Left (APIError (RateLimitError msg)) -> do
      Reddit $ liftIO $ print msg
      onLimit
      rateLimitWith onLimit act
    Left x -> Reddit . EitherT . return $ Left x
    Right x -> Reddit $ return x

-- | Wait @n@ seconds. Essentially @threadDelay@ in the @Reddit@ type using seconds
--   instead of microseconds.
wait :: Int -> Reddit ()
wait = liftIO . threadDelay . (* 1000000)