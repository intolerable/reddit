module Reddit.API
  ( module Export
  , runReddit
  , runRedditWithRateLimiting ) where

import Reddit.API.Actions as Export
import Reddit.API.Login
import Reddit.API.Types.Error as Export
import Reddit.API.Types.Reddit as Export

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
run user pass shouldRateLimit (RedditT reddit) =
  runAPI builder (shouldRateLimit, Nothing) $ do
    customizeRequest addHeader
    LoginDetails (Modhash mh) cj <- unRedditT $ login user pass
    customizeRequest $ \r ->
      addHeader r { cookieJar = Just cj
                  , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit
