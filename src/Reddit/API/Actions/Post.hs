module Reddit.API.Actions.Post where

import Reddit.API.Routes as Route
import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Comment
import Reddit.API.Types.Empty
import Reddit.API.Types.Listing
import Reddit.API.Types.Reddit

import Control.Monad.IO.Class
import Data.Text (Text)

getPostInfo :: MonadIO m => PostID -> RedditT m Post
getPostInfo pID = do
  Listing _ _ (p:[]) <- runRoute $ Route.aboutPost pID :: MonadIO m => RedditT m PostListing
  return p

getHotPosts :: MonadIO m => RedditT m PostListing
getHotPosts = runRoute $ Route.postsListing Nothing "hot"

getHotSubredditPosts :: MonadIO m => SubredditName -> RedditT m PostListing
getHotSubredditPosts r = runRoute $ Route.postsListing (Just r) "hot"

getNewPosts :: MonadIO m => RedditT m PostListing
getNewPosts = runRoute $ Route.postsListing Nothing "new"

getNewSubredditPosts :: MonadIO m => SubredditName -> RedditT m PostListing
getNewSubredditPosts r = runRoute $ Route.postsListing (Just r) "new"

savePost :: MonadIO m => PostID -> RedditT m ()
savePost = nothing . runRoute . Route.savePost

unsavePost :: MonadIO m => PostID -> RedditT m ()
unsavePost = nothing . runRoute . Route.unsavePost

submitLink :: MonadIO m => SubredditName -> Text -> Text -> RedditT m PostID
submitLink r title url = do
  POSTWrapped res <- runRoute $ Route.submitLink r title url
  return res

deletePost :: MonadIO m => PostID -> RedditT m ()
deletePost = nothing . runRoute . Route.deletePost

getComments :: MonadIO m => PostID -> RedditT m [CommentReference]
getComments p = do
  PostComments _ c <- runRoute $ Route.getComments p
  return c

enableReplies :: MonadIO m => PostID -> RedditT m ()
enableReplies = nothing . runRoute . Route.sendReplies True

disableReplies :: MonadIO m => PostID -> RedditT m ()
disableReplies = nothing . runRoute . Route.sendReplies False

removePost :: MonadIO m => PostID -> RedditT m ()
removePost = nothing . runRoute . Route.removePost False

markPostAsSpam :: MonadIO m => PostID -> RedditT m ()
markPostAsSpam = nothing . runRoute . Route.removePost True
