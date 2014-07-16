module Reddit.API.Routes.Run
  ( runRoute ) where

import Reddit.API.Types.Reddit

import APIBuilder.Routes (Route)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad.Trans.State
import Data.Aeson (FromJSON)
import Data.DateTime (DateTime)
import Network.HTTP.Conduit
import qualified APIBuilder as API
import qualified Data.DateTime as DateTime

runRoute :: (MonadIO m, FromJSON a) => Route -> RedditT m a
runRoute route = do
  (limiting, rli) <- RedditT $ API.liftState get
  whenJust rli $ \r -> when (needsReset r && limiting) $ waitForReset (resetTime r)
  resp <- RedditT $ API.routeResponse route
  time <- liftIO DateTime.getCurrentTime
  whenJust (headersToRateLimitInfo (responseHeaders resp) time) $ \r ->
    RedditT $ API.liftState $ put (limiting, Just r)
  RedditT $ hoistEither $ API.decode $ responseBody resp

waitForReset :: MonadIO m => DateTime -> RedditT m ()
waitForReset dt = do
  time <- liftIO DateTime.getCurrentTime
  let wait = fromIntegral $ DateTime.diffSeconds dt time
  liftIO $ threadDelay $ (wait + 5) * 1000000 -- wait five extra seconds to account for timing differences

needsReset :: RateLimitInfo -> Bool
needsReset = (<= 0) . remaining

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing _ = return ()
