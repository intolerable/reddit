module Reddit.API.Types.Reddit
  ( Reddit(..)
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
import Data.Aeson
import Data.DateTime
import Data.Monoid (mempty)
import Data.Text (Text)
import Network.HTTP.Types
import Network.HTTP.Conduit hiding (path)

newtype Reddit a = Reddit { unReddit :: API () RedditError a }

instance Functor Reddit where
  fmap f (Reddit a) = Reddit (fmap f a)

instance Applicative Reddit where
  pure a = Reddit (pure a)
  (Reddit f) <*> (Reddit a) = Reddit (f <*> a)

instance Monad Reddit where
  return a = Reddit (return a)
  (Reddit a) >>= f = Reddit (a >>= unReddit . f)

instance MonadIO Reddit where
  liftIO a = Reddit (liftIO a)

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
