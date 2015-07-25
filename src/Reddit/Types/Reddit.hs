module Reddit.Types.Reddit
  ( Reddit
  , RedditT(..)
  , RedditF(..)
  , runRoute
  , receiveRoute
  , nest
  , failWith
  , withBaseURL
  , Modhash(..)
  , LoginDetails(..)
  , POSTWrapped(..)
  , ShouldRateLimit
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
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.API.Builder hiding (runRoute)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types
import Prelude
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS

type Reddit a = RedditT IO a

--newtype RedditT m a = RedditT { unRedditT :: APIT (TVar RateLimits) RedditError m a }

--instance Functor m => Functor (RedditT m) where
--  fmap f (RedditT a) = RedditT (fmap f a)

--instance (Functor m, Monad m) => Applicative (RedditT m) where
--  pure a = RedditT (pure a)
--  (RedditT f) <*> (RedditT a) = RedditT (f <*> a)

--instance Monad m => Monad (RedditT m) where
--  return a = RedditT (return a)
--  (RedditT a) >>= f = RedditT (a >>= unRedditT . f)
--  fail = failWith . APIError . FailError . Text.pack

--instance MonadIO m => MonadIO (RedditT m) where
--  liftIO a = RedditT (liftIO a)

--instance MonadTrans RedditT where
--  lift = RedditT . lift . lift . lift . lift

data RedditF m a where
  FailWith :: APIError RedditError -> RedditF m a
  Nest :: RedditT m b -> (Either (APIError RedditError) b -> a) -> RedditF m a
  NestResuming :: RedditT m b -> (Either (APIError RedditError, Maybe (RedditT m b)) b -> a) -> RedditF m a
  ReceiveRoute :: Receivable b => Route -> (b -> a) -> RedditF m a
  RunRoute :: FromJSON b => Route -> (b -> a) -> RedditF m a
  WithBaseURL :: Text -> RedditT m b -> (b -> a) -> RedditF m a

instance Functor (RedditF m) where
  fmap _ (FailWith x) = FailWith x
  fmap f (Nest a x) = Nest a (fmap f x)
  fmap f (NestResuming a x) = NestResuming a (fmap f x)
  fmap f (ReceiveRoute r x) = ReceiveRoute r (fmap f x)
  fmap f (RunRoute r x) = RunRoute r (fmap f x)
  fmap f (WithBaseURL u a x) = WithBaseURL u a (fmap f x)

newtype RedditT m a = RedditT (FreeT (RedditF m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans RedditT where
  lift = RedditT . lift

instance MonadIO m => MonadIO (RedditT m) where
  liftIO = RedditT . liftIO

runRoute :: (FromJSON a, Monad m) => Route -> RedditT m a
runRoute r = RedditT $ liftF $ RunRoute r id

receiveRoute :: (Receivable a, Monad m) => Route -> RedditT m a
receiveRoute r = RedditT $ liftF $ ReceiveRoute r id

nest :: Monad m => RedditT m a -> RedditT m (Either (APIError RedditError) a)
nest f = RedditT $ liftF $ Nest f id

withBaseURL :: Monad m => Text -> RedditT m a -> RedditT m a
withBaseURL u f = RedditT $ liftF $ WithBaseURL u f id

failWith :: Monad m => APIError RedditError -> RedditT m a
failWith = RedditT . liftF . FailWith

newtype Modhash = Modhash Text
  deriving (Show, Read, Eq)

instance FromJSON Modhash where
  parseJSON (Object o) =
    Modhash <$> ((o .: "json") >>= (.: "data") >>= (.: "modhash"))
  parseJSON _ = mempty

data LoginDetails = LoginDetails Modhash CookieJar
  deriving (Show, Eq)

instance Receivable LoginDetails where
  receive x = do
    (resp, mh) <- receive x
    return $ LoginDetails (unwrapJSON mh) (responseCookieJar (resp :: Response ByteString))

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
        rlResetTime' = fmap (\s -> addUTCTime (fromIntegral s) now) rlResetTime
        extract s = lookup s hs >>= readMaybe . BS.unpack
        trimap f (a, b, c) = (f a, f b, f c)

builder :: Builder
builder = Builder "Reddit"
                  mainBaseURL
                  addAPIType
                  (addHeader Nothing)

addHeader :: Maybe BS.ByteString -> Request -> Request
addHeader Nothing req = req { requestHeaders =
  ("User-Agent", "reddit-haskell 0.1.0.0 / intolerable") : requestHeaders req }
addHeader (Just hdr) req = req { requestHeaders =
  ("User-Agent", hdr) : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ("api_type" =. ("json" :: Text) : ps) m

mainBaseURL :: Text
mainBaseURL = "https://api.reddit.com"

loginBaseURL :: Text
loginBaseURL = "https://ssl.reddit.com"
