-- | This module should be most of what you need to operate the library.
--   It exports functionality for running built 'RedditT' actions, as well
--   as re-exporting a few helpful types from around the library. Not every
--   type is exported, however, due to clashing record fields. It's recommended
--   to import modules from @Reddit.Types.*@ qualified so that you can use all
--   the record fields without having to deal with ambiguous functions.
module Reddit
  ( runReddit
  , runRedditAnon
  , runRedditWith
  , RedditOptions(..)
  , LoginMethod(..)
  -- * Re-exports
  , APIError(..)
  , module Reddit.Actions
  , module Reddit.Types
  , module Reddit.Types.Error
  , module Reddit.Types.Reddit ) where

import Reddit.Actions
import Reddit.Login
import Reddit.Types.Error
import Reddit.Types
import Reddit.Types.Reddit hiding (info, should)

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.API.Builder
import Network.HTTP.Conduit

-- | Options for how we should run the 'Reddit' action.
--
-- - 'rateLimitingEnabled': 'True' if the connection should be automatically rate-limited
--   and should pause when we hit the limit, 'False' otherwise.
--
-- - 'connectionManager': @'Just' x@ if the connection should use the 'Manager' @x@, 'Nothing'
--   if we should create a new one for the connection.
--
-- - 'loginMethod': The method we should use for authentication, described in 'LoginMethod'.
--
-- - 'customUserAgent': @'Just' "string"@ if the connection should use the user agent @"string"@,
--   @'Nothing'@ if it should use the default agent.
data RedditOptions =
  RedditOptions { rateLimitingEnabled :: Bool
                , connectionManager :: Maybe Manager
                , loginMethod :: LoginMethod
                , customUserAgent :: Maybe ByteString }

instance Default RedditOptions where
  def = RedditOptions True Nothing Anonymous Nothing

-- | Should we log in to Reddit? If so, should we use a stored set of credentials
--   or get a new fresh set?
data LoginMethod = Anonymous -- ^ Don't login, instead use an anonymous account
                 | Credentials Text Text -- ^ Login using the specified username and password
                 | StoredDetails LoginDetails -- ^
                 --   Login using a stored set of credentials. Usually the best way to get
                 --   these is to do @'runRedditAnon' $ 'login' user pass@.
  deriving (Show)

instance Default LoginMethod where def = Anonymous

-- | Run a 'Reddit' action (or a 'RedditT' transformer action). This uses the default logged-in settings
--   for 'RedditOptions': rate limiting enabled, default manager, login via username and password, and
--   the default user-agent. You should change the user agent if you're making anything more complex than
--   a basic script, since Reddit's API policy says that you should have a uniquely identifiable user agent.
runReddit :: MonadIO m => Text -> Text -> RedditT m a -> m (Either (APIError RedditError) a)
runReddit user pass = runRedditWith def { loginMethod = Credentials user pass }

-- | Run a 'Reddit' action (or a 'RedditT' transformer action). This uses the default logged-out settings, so
--   you won't be able to do anything that requires authentication (like checking messages or making a post).
--   At the moment, authentication isn't statically checked, so it'll return a runtime error if you try to do
--   anything you don't have permissions for.
runRedditAnon :: MonadIO m => RedditT m a -> m (Either (APIError RedditError) a)
runRedditAnon = runRedditWith def

-- | Run a 'Reddit' or 'RedditT' action with custom settings. You probably won't need this function for
--   most things, but it's handy if you want to persist a connection over multiple 'Reddit' sessions or
--   use a custom user agent string.
runRedditWith :: MonadIO m => RedditOptions -> RedditT m a -> m (Either (APIError RedditError) a)
runRedditWith (RedditOptions rl man lm ua) (RedditT reddit) = do
  rli <- liftIO $ newTVarIO $ RateLimits rl Nothing
  manager <- case man of
    Just m -> return m
    Nothing -> liftIO $ newManager conduitManagerSettings
  (res, _, _) <- runAPI builder manager rli $ do
    customizeRequest $ addHeader ua
    case lm of
      StoredDetails (LoginDetails (Modhash mh) cj) ->
        customizeRequest $ \r ->
          addHeader ua r { cookieJar = Just cj
                         , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
      Credentials user pass -> do
        LoginDetails (Modhash mh) cj <- unRedditT $ login user pass
        customizeRequest $ \r ->
          addHeader ua r { cookieJar = Just cj
                         , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
      Anonymous -> return ()
    reddit
  return res
