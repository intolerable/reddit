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

runBots :: [Bot] -> Text -> Text -> IO (Either (APIError RedditError) ())
runBots bs user pass = runReddit user pass $ unBot $ mconcat bs

every :: Int -> Reddit a -> Reddit ()
every n act = forever $ void act >> wait n

rateLimit :: Reddit a -> Reddit a
rateLimit = rateLimitEvery 60 

forkReddit :: Reddit a -> Reddit ()
forkReddit a = do
  io <- makeIO a
  void $ liftIO $ forkIO $ void io

rateLimitEvery :: Int -> Reddit a -> Reddit a
rateLimitEvery duration act = do
  res <- nest act
  case res of 
    Left (APIError (RateLimitError msg)) -> do
      liftIO $ print msg
      wait duration
      rateLimit act
    Left x -> EitherT . return $ Left x
    Right x -> return x

wait :: Int -> Reddit ()
wait = liftIO . threadDelay . (* 1000000)