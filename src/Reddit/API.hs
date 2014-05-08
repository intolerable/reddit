module Reddit.API
  ( module Export
  , runReddit
  , nest
  , makeIO ) where

import Reddit.API.Actions as Export
import Reddit.API.Login
import Reddit.API.Types.Reddit as Export
import Reddit.API.Types.Error as Export

import APIBuilder
import APIBuilder as Export (APIError(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit

runReddit :: Text -> Text -> Reddit a -> IO (Either (APIError RedditError) a)
runReddit user pass (Reddit reddit) =
  runAPI builder () $ do
    customizeRequest addHeader
    LoginDetails (Modhash mh) cj <- unReddit $ login user pass
    customizeRequest $ \r ->
      addHeader r { cookieJar = Just cj
                  , requestHeaders = ("X-Modhash", encodeUtf8 mh):requestHeaders r }
    reddit

nest :: Reddit a -> Reddit (Either (APIError RedditError) a)
nest (Reddit a) = do
  b <- Reddit $ liftBuilder get
  liftIO $ runAPI b () a

makeIO :: Reddit a -> Reddit (IO (Either (APIError RedditError) a))
makeIO (Reddit a) = do
  b <- Reddit $ liftBuilder get
  return $ runAPI b () a