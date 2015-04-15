module Reddit.Types.Reddit
  ( Reddit
  , RedditT(..)
  , nest
  , failWith
  , Modhash(..)
  , LoginDetails(..)
  , POSTWrapped(..)
  , RateLimits(RateLimits, should, info)
  , RateLimitInfo(..)
  , headersToRateLimitInfo
  , builder
  , mainBaseURL
  , loginBaseURL
  , addHeader
  , addAPIType ) where

import Reddit.Types.Error

import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State (get, put)
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time.Clock
import Network.API.Builder
import Network.HTTP.Conduit hiding (path)
import Network.HTTP.Types
import Prelude hiding (mempty)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

type Reddit a = RedditT IO a

newtype RedditT m a = RedditT { unRedditT :: APIT (TVar RateLimits) RedditError m a }

instance Monad m => Functor (RedditT m) where
  fmap f (RedditT a) = RedditT (fmap f a)

instance Monad m => Applicative (RedditT m) where
  pure a = RedditT (pure a)
  (RedditT f) <*> (RedditT a) = RedditT (f <*> a)

instance Monad m => Monad (RedditT m) where
  return a = RedditT (return a)
  (RedditT a) >>= f = RedditT (a >>= unRedditT . f)
  fail = failWith . APIError . FailError . Text.pack

instance MonadIO m => MonadIO (RedditT m) where
  liftIO a = RedditT (liftIO a)

instance MonadTrans RedditT where
  lift = RedditT . lift . lift . lift . lift

nest :: MonadIO m => RedditT m a -> RedditT m (Either (APIError RedditError) a)
nest (RedditT a) = do
  b <- RedditT $ liftBuilder get
  rl <- RedditT $ liftState get
  m <- RedditT $ liftManager ask
  (res, b', rl') <- lift $ runAPI b m rl a
  RedditT $ do
    liftBuilder $ put b'
    liftState $ put rl'
  return res

failWith :: Monad m => APIError RedditError -> RedditT m a
failWith = RedditT . EitherT . return . Left

newtype Modhash = Modhash Text
  deriving (Show, Read, Eq)

instance FromJSON Modhash where
  parseJSON (Object o) =
    Modhash <$> ((o .: "json") >>= (.: "data") >>= (.: "modhash"))
  parseJSON _ = mempty

data LoginDetails = LoginDetails Modhash CookieJar
  deriving (Show, Eq)

newtype POSTWrapped a = POSTWrapped a
  deriving (Show, Read, Eq)

instance Functor POSTWrapped where
  fmap f (POSTWrapped a) = POSTWrapped (f a)

data RateLimits =
  RateLimits { should :: ShouldRateLimit
             , info :: Maybe RateLimitInfo }
  deriving (Show, Read, Eq)

type ShouldRateLimit = Bool

data RateLimitInfo = RateLimitInfo { used :: Integer
                                   , remaining :: Integer
                                   , resetTime :: UTCTime }
  deriving (Show, Read, Eq)

headersToRateLimitInfo :: ResponseHeaders -> UTCTime -> Maybe RateLimitInfo
headersToRateLimitInfo hs now =
  RateLimitInfo <$> rlUsed <*> rlRemaining <*> rlResetTime'
  where (rlUsed, rlRemaining, rlResetTime) =
          trimap extract ("x-ratelimit-used", "x-ratelimit-remaining", "x-ratelimit-reset")
        rlResetTime' = fmap (\s -> addUTCTime (fromIntegral s) now) $ rlResetTime
        extract s = lookup s hs >>= readMaybe . BS.unpack
        trimap f (a, b, c) = (f a, f b, f c)

builder :: Builder
builder = Builder "Reddit"
                  mainBaseURL
                  addAPIType
                  addHeader

addHeader :: Request -> Request
addHeader req = req { requestHeaders =
  ("User-Agent","reddit-haskell 0.1.0.0 / intolerable") : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ("api_type" =. ("json" :: Text) : ps) m

mainBaseURL :: Text
mainBaseURL = "https://api.reddit.com"

loginBaseURL :: Text
loginBaseURL = "https://ssl.reddit.com"
