module Reddit.API
  ( module Export
  , runReddit
  , nest ) where

import Reddit.API.Actions as Export
import Reddit.API.Login
import Reddit.API.Types.Reddit as Export
import Reddit.API.Types.Error as Export

import APIBuilder
import APIBuilder as Export (APIError(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit

runReddit :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runReddit user pass (RedditT reddit) =
  runAPI builder () $ do
    customizeRequest addHeader
    LoginDetails (Modhash mh) cj <- unRedditT $ login user pass
    customizeRequest $ \r ->
      addHeader r { cookieJar = Just cj
                  , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit

nest :: MonadIO m => RedditT m a -> RedditT m (Either (APIError RedditError) a)
nest (RedditT a) = do
  b <- RedditT $ liftBuilder get
  lift $ runAPI b () a
