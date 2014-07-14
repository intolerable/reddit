module Reddit.API.Login
  ( module Reddit.API.Login ) where

import Reddit.API.Types.Reddit

import APIBuilder
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Text (Text)
import Network.HTTP.Conduit

loginRoute :: Text -> Text -> Route
loginRoute user pass = Route [ "api", "login" ]
                             [ "rem" =. ("true" :: Text)
                             , "user" =. user
                             , "passwd" =. pass ]
                             "POST"

getLoginDetails :: MonadIO m => Text -> Text -> RedditT m LoginDetails
getLoginDetails user pass = do
  b <- RedditT $ liftBuilder get
  req <- RedditT . EitherT . return $ case routeRequest b (loginRoute user pass) of
    Just url -> Right url
    Nothing -> Left InvalidURLError
  resp <- liftIO $ withManager $ httpLbs req
  let cj = responseCookieJar resp
  mh <- RedditT . EitherT . return . decode $ responseBody resp
  return $ LoginDetails mh cj

login :: MonadIO m => Text -> Text -> RedditT m LoginDetails
login user pass = do
  RedditT $ baseURL loginBaseURL
  d <- getLoginDetails user pass
  RedditT $ baseURL mainBaseURL
  return d
