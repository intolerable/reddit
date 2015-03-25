module Reddit.Actions.Thing
  ( Reddit.Actions.Thing.reply
  , Reddit.Actions.Thing.delete
  , Reddit.Actions.Thing.report ) where

import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import qualified Reddit.Routes.Thing as Route

import Control.Monad.IO.Class
import Data.Text (Text)

reply :: (MonadIO m, Thing a) => a -> Text -> RedditT m CommentID
reply t b = do
  POSTWrapped res <- runRoute $ Route.reply t b
  return res

delete :: (MonadIO m, Thing a) => a -> RedditT m ()
delete = nothing . runRoute . Route.delete

report :: (MonadIO m, Thing a) => a -> Text -> RedditT m ()
report t r = nothing $ runRoute $ Route.report t r
