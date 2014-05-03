module Reddit.API.Types.Reddit
  ( Reddit
  , Modhash(..)
  , LoginDetails(..)
  , POSTWrapped(..)
  , builder
  , mainBaseURL
  , loginBaseURL
  , addHeader
  , addAPIType ) where

import Reddit.API.Types.Error

import APIBuilder
import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text)
import Network.HTTP.Conduit hiding (path)

type Reddit a = API () RedditError a

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

--instance FromJSON a => FromJSON (POSTWrapped a) where
--  parseJSON (Object o) = do
--    d <- (o .: "json") >>= (.: "data")
--    i <- d .:? "id"
--    case i of
--      Just v -> POSTWrapped <$> parseJSON v
--      Nothing -> do
--        things <- d .: "things"
--        case traceShow things things of 
--          Array ts -> case ts !? 0 of
--            Just h -> do 
--              commentid <- h .: "id"
--              return $ POSTWrapped $ commentid 
--            Nothing -> mempty
--          _ -> mempty
--  parseJSON _ = mempty 

builder :: Builder
builder = Builder "Reddit API"
                  mainBaseURL
                  addAPIType
                  addHeader

addHeader :: Request -> Request
addHeader req = req { requestHeaders = ("User-Agent","reddit-haskell") : requestHeaders req }

addAPIType :: Route -> Route
addAPIType (Route fs ps m) = Route fs ("api_type" =. Just "json" : ps) m 

mainBaseURL :: Text
mainBaseURL = "http://api.reddit.com"

loginBaseURL :: Text
loginBaseURL = "https://ssl.reddit.com"