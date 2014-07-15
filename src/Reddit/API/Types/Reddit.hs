module Reddit.API.Types.Reddit
  ( Reddit
  , RedditT(..)
  , Modhash(..)
  , LoginDetails(..)
  , POSTWrapped(..)
  , RateLimitInfo(..)
  , builder
  , mainBaseURL
  , loginBaseURL
  , addHeader
  , addAPIType ) where

import Reddit.API.Types.Error

import APIBuilder
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import Data.DateTime
import Data.Monoid (mempty)
import Data.Text (Text)
import Network.HTTP.Conduit hiding (path)
import Network.HTTP.Types

type Reddit a = RedditT IO a

newtype RedditT m a = RedditT { unRedditT :: APIT () RedditError m a }

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

headersToRateLimitInfo :: ResponseHeaders -> Maybe RateLimitInfo
headersToRateLimitInfo = undefined

builder :: Builder
builder = Builder "Reddit API"
                  mainBaseURL
                  addAPIType
                  addHeader

addHeader :: Request -> Request
addHeader req = req { requestHeaders = ("User-Agent","reddit-haskell") : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ("api_type" =. ("json" :: Text) : ps) m

mainBaseURL :: Text
mainBaseURL = "http://api.reddit.com"

loginBaseURL :: Text
loginBaseURL = "https://ssl.reddit.com"
