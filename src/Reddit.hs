module Reddit
  ( module Export
  , runReddit
  , runRedditWithRateLimiting ) where

import Reddit.Actions as Export
import Reddit.Login
import Reddit.Types.Error as Export
import Reddit.Types.Reddit as Export hiding (info, should)

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Builder
import Network.API.Builder as Export (APIError(..))
import Network.HTTP.Conduit

runReddit :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runReddit user pass = run user pass False

runRedditWithRateLimiting :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runRedditWithRateLimiting user pass = run user pass True

run :: MonadIO m => Text -> Text -> Bool -> RedditT m a -> m (Either (APIError RedditError) a)
run user pass shouldRateLimit (RedditT reddit) = do
  rli <- liftIO $ newTVarIO $ RateLimits shouldRateLimit Nothing
  execAPI builder rli $ do
    customizeRequest addHeader
    LoginDetails (Modhash mh) cj <- unRedditT $ login user pass
    customizeRequest $ \r ->
      addHeader r { cookieJar = Just cj
                  , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit
