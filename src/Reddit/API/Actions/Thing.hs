module Reddit.API.Actions.Thing where

import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.Thing as Route
import Reddit.API.Routes.Run

import Control.Monad.IO.Class
import Data.Text (Text)

reply :: (MonadIO m, Thing a) => a -> Text -> RedditT m CommentID
reply t b = do
  POSTWrapped res <- runRoute $ Route.reply t b
  return $ res

delete :: (MonadIO m, Thing a) => a -> RedditT m ()
delete = nothing . runRoute . Route.delete
