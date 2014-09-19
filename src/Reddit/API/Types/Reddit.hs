module Reddit.API.Types.Reddit
  ( Reddit
  , RedditT(..)
  , nest
  , Modhash(..)
  , LoginDetails(..)
  , POSTWrapped(..)
  , RateLimitInfo(..)
  , headersToRateLimitInfo
  , builder
  , mainBaseURL
  , loginBaseURL
  , addHeader
  , addAPIType ) where

import Reddit.API.Types.Error

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State (get)
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Monoid (mempty)
import Data.Text (Text)
import Network.API.Builder
import Network.HTTP.Conduit hiding (path)
import Network.HTTP.Types
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.DateTime as DateTime

type Reddit a = RedditT IO a

newtype RedditT m a = RedditT { unRedditT :: APIT (Bool, Maybe RateLimitInfo) RedditError m a }

instance Monad m => Functor (RedditT m) where
  fmap f (RedditT a) = RedditT (fmap f a)

instance Monad m => Applicative (RedditT m) where
  pure a = RedditT (pure a)
  (RedditT f) <*> (RedditT a) = RedditT (f <*> a)

instance Monad m => Monad (RedditT m) where
  return a = RedditT (return a)
  (RedditT a) >>= f = RedditT (a >>= unRedditT . f)

instance MonadIO m => MonadIO (RedditT m) where
  liftIO a = RedditT (liftIO a)

instance MonadTrans RedditT where
  lift a = RedditT (lift . lift . lift $ a)

nest :: MonadIO m => RedditT m a -> RedditT m (Either (APIError RedditError) a)
nest (RedditT a) = do
  b <- RedditT $ liftBuilder get
  rl <- RedditT $ liftState get
  lift $ execAPI b rl a

newtype Modhash = Modhash Text
  deriving (Show, Read, Eq)

instance FromJSON Modhash where
  parseJSON (Object o) =
    Modhash <$> ((o .: "json") >>= (.: "data") >>= (.: "modhash"))
  parseJSON _ = mempty

data LoginDetails = LoginDetails Modhash CookieJar
  deriving (Show, Eq)

data POSTWrapped a = POSTWrapped a
  deriving (Show, Read, Eq)

data RateLimitInfo = RateLimitInfo { used :: Integer
                                   , remaining :: Integer
                                   , resetTime :: DateTime }
  deriving (Show, Read, Eq)

headersToRateLimitInfo :: ResponseHeaders -> DateTime -> Maybe RateLimitInfo
headersToRateLimitInfo hs now =
  RateLimitInfo <$> rlUsed <*> rlRemaining <*> rlResetTime'
  where (rlUsed, rlRemaining, rlResetTime) =
          trimap extract ("x-ratelimit-used", "x-ratelimit-remaining", "x-ratelimit-reset")
        rlResetTime' = fmap (`DateTime.addSeconds` now) rlResetTime
        extract s = lookup s hs >>= readMaybe . BS.unpack
        trimap f (a, b, c) = (f a, f b, f c)

builder :: Builder
builder = Builder "Reddit API"
                  mainBaseURL
                  addAPIType
                  addHeader

addHeader :: Request -> Request
addHeader req = req { requestHeaders =
  ("User-Agent","reddit-haskell 0.1.0.0 / intolerable") : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ("api_type" =. ("json" :: Text) : ps) m

mainBaseURL :: Text
mainBaseURL = "http://api.reddit.com"

loginBaseURL :: Text
loginBaseURL = "https://ssl.reddit.com"
