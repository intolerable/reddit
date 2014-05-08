module Reddit.API.Login
  ( module Reddit.API.Login ) where

import Reddit.API.Types.Reddit

import APIBuilder
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Data.Text (Text)
import Network.HTTP.Conduit

loginRoute :: Text -> Text -> Route
loginRoute user pass = Route [ "api", "login" ]
                             [ "rem" =. Just "true"
                             , "user" =. Just user
                             , "passwd" =. Just pass ]
                             POST

getLoginDetails :: Text -> Text -> Reddit LoginDetails
getLoginDetails user pass = do
  b <- Reddit $ liftBuilder get
  req <- Reddit . EitherT . return $ case routeRequest b (loginRoute user pass) of
    Just url -> Right url
    Nothing -> Left InvalidURLError
  resp <- liftIO $ withManager $ httpLbs req 
  let cj = responseCookieJar resp
  mh <- Reddit . EitherT . return . decode $ responseBody resp
  return $ LoginDetails mh cj

login :: Text -> Text -> Reddit LoginDetails
login user pass = do
  Reddit $ baseURL loginBaseURL
  d <- getLoginDetails user pass
  Reddit $ baseURL mainBaseURL
  return d
