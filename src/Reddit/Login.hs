module Reddit.Login
  ( login ) where

import Reddit.Types.Error
import Reddit.Types.Reddit

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Text (Text)
import Network.API.Builder
import Network.HTTP.Conduit

loginRoute :: Text -> Text -> Route
loginRoute user pass = Route [ "api", "login" ]
                             [ "rem" =. True
                             , "user" =. user
                             , "passwd" =. pass ]
                             "POST"

getLoginDetails :: MonadIO m => Text -> Text -> RedditT m LoginDetails
getLoginDetails user pass = do
  resp <- RedditT $ sendRoute () $ loginRoute user pass
  let cj = responseCookieJar resp
  case unwrapJSON `fmap` receive resp of
    Left x@(APIError (RateLimitError wait _)) -> do
      RateLimits limiting _ <- RedditT $ liftState get >>= liftIO . readTVarIO
      if limiting
        then do
          liftIO $ threadDelay $ (fromIntegral wait + 5) * 1000000
          getLoginDetails user pass
        else RedditT $ ExceptT $ return $ Left x
    Left x -> RedditT $ ExceptT $ return $ Left x
    Right modhash -> return $ LoginDetails modhash cj


-- | Make a login request with the given username and password.
login :: MonadIO m
      => Text -- ^ Username to login with
      -> Text -- ^ Password to login with
      -> RedditT m LoginDetails
login user pass = do
  RedditT $ baseURL loginBaseURL
  d <- getLoginDetails user pass
  RedditT $ baseURL mainBaseURL
  return d
