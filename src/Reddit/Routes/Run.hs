module Reddit.Routes.Run
  ( runRoute ) where

import Reddit.Types.Error
import Reddit.Types.Reddit

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson (FromJSON(..))
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock
import Network.API.Builder.Routes (Route)
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Network.API.Builder as API

runRoute :: (MonadIO m, FromJSON a) => Route -> RedditT m a
runRoute route = do
  RateLimits limiting _ <- RedditT $ API.liftState get >>= liftIO . readTVarIO
  if limiting
    then runRouteWithLimiting route
    else RedditT $ API.unwrapJSON `liftM` API.runRoute route

runRouteWithLimiting :: (MonadIO m, FromJSON a) => Route -> RedditT m a
runRouteWithLimiting route = do
  RateLimits _ rli <- RedditT $ API.liftState get >>= liftIO . readTVarIO
  case rli of
    Just r -> do
      when (needsReset r) $ waitForReset $ resetTime r
      requestAndUpdate
    Nothing -> requestAndUpdate
  where
    requestAndUpdate = do
      resp <- nest $ RedditT $ API.routeResponse route
      case resp of
        Left (API.APIError (RateLimitError resetIn _)) -> do
          updateFromZero resetIn
          runRouteWithLimiting route
        Left x -> RedditT $ ExceptT $ return $ Left x
        Right x -> do
          case API.unwrapJSON <$> API.receive x of
            Left (API.APIError (RateLimitError resetIn _)) -> do
              updateFromZero resetIn
              runRouteWithLimiting route
            Left y -> RedditT $ ExceptT $ return $ Left y
            Right y -> do
              updateRateLimitInfo $ responseHeaders x
              return y

decodeFromResponse :: (Monad m, FromJSON a) => Response ByteString -> RedditT m a
decodeFromResponse = RedditT . ExceptT . return . fmap API.unwrapJSON . API.receive

updateRateLimitInfo :: MonadIO m => ResponseHeaders -> RedditT m ()
updateRateLimitInfo hs = do
  time <- liftIO getCurrentTime
  case headersToRateLimitInfo hs time of
    Just rli ->
      RedditT $ do
        r <- API.liftState get
        liftIO $ atomically $ modifyTVar r (\(RateLimits b _) -> RateLimits b $ Just rli)
    Nothing -> return ()

updateFromZero :: MonadIO m => Integer -> RedditT m ()
updateFromZero resetIn = do
  time <- liftIO getCurrentTime
  let resetAt = addUTCTime (fromInteger resetIn) time
  RedditT $ do
    r <- API.liftState get
    liftIO $ atomically $ modifyTVar r (\(RateLimits b _) -> RateLimits b $ Just $ RateLimitInfo 0 0 resetAt)

waitForReset :: MonadIO m => UTCTime -> RedditT m ()
waitForReset dt = do
  time <- liftIO getCurrentTime
  let wait = floor $ diffUTCTime dt time
  liftIO $ threadDelay $ (wait + 5) * 1000000 -- wait five extra seconds to account for timing differences

needsReset :: RateLimitInfo -> Bool
needsReset = (<= 0) . remaining
