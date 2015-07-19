module Reddit
  ( module Export
  , runReddit
  , RedditOptions(..)
  , runRedditWith ) where

import Reddit.Actions as Export
import Reddit.Login
import Reddit.Types.Error as Export
import Reddit.Types.Reddit as Export hiding (info, should)

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Builder
import Network.API.Builder as Export (APIError(..))
import Network.HTTP.Conduit

data RedditOptions =
  RedditOptions { rateLimitingEnabled :: Bool
                , connectionManager :: Maybe Manager
                , loginDetails :: Maybe LoginDetails
                , customUserAgent :: Maybe ByteString }

instance Default RedditOptions where
  def = RedditOptions True Nothing Nothing Nothing

runReddit :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runReddit user pass = runRedditWith user pass def

runRedditWith :: MonadIO m => Text -> Text -> RedditOptions -> RedditT m a -> m (Either (APIError RedditError) a)
runRedditWith user pass (RedditOptions rl man ld ua) (RedditT reddit) = do
  rli <- liftIO $ newTVarIO $ RateLimits rl Nothing
  manager <- case man of
    Just m -> return m
    Nothing -> liftIO $ newManager conduitManagerSettings
  (res, _, _) <- runAPI builder manager rli $ do
    customizeRequest $ addHeader ua
    LoginDetails (Modhash mh) cj <- case ld of
      Just l -> return l
      Nothing -> unRedditT $ login user pass
    customizeRequest $ \r ->
      addHeader ua r { cookieJar = Just cj
                     , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit
  return res
