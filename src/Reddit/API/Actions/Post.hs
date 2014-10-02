module Reddit.API.Actions.Post where

import Reddit.API.Routes as Route
import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Comment
import Reddit.API.Types.Empty
import Reddit.API.Types.Listing
import Reddit.API.Types.Options
import Reddit.API.Types.Reddit

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Default

getPostInfo :: MonadIO m => PostID -> RedditT m Post
getPostInfo pID = do
  Listing _ _ (p:[]) <- runRoute $ Route.aboutPost pID :: MonadIO m => RedditT m PostListing
  return p

getHotPosts' :: MonadIO m => Options PostID -> RedditT m PostListing
getHotPosts' opts = runRoute $ Route.postsListing opts Nothing "hot"

getHotPosts :: MonadIO m => RedditT m PostListing
getHotPosts = getHotPosts' def

getHotSubredditPosts' :: MonadIO m => Options PostID -> SubredditName -> RedditT m PostListing
getHotSubredditPosts' opts r = runRoute $ Route.postsListing opts (Just r) "hot"

getHotSubredditPosts :: MonadIO m => SubredditName -> RedditT m PostListing
getHotSubredditPosts = getHotSubredditPosts' def

getNewPosts' :: MonadIO m => Options PostID -> RedditT m PostListing
getNewPosts' opts = runRoute $ Route.postsListing opts Nothing "new"

getNewPosts :: MonadIO m => RedditT m PostListing
getNewPosts = getNewPosts' def

getNewSubredditPosts' :: MonadIO m => Options PostID -> SubredditName -> RedditT m PostListing
getNewSubredditPosts' opts r = runRoute $ Route.postsListing opts (Just r) "new"

getNewSubredditPosts :: MonadIO m => SubredditName -> RedditT m PostListing
getNewSubredditPosts = getNewSubredditPosts' def

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

getPostComments :: MonadIO m => PostID -> RedditT m PostComments
getPostComments = runRoute . Route.getComments

getComments :: MonadIO m => PostID -> RedditT m [CommentReference]
getComments p = do
  PostComments _ c <- getPostComments p
  return c

enableReplies :: MonadIO m => PostID -> RedditT m ()
enableReplies = nothing . runRoute . Route.sendReplies True

disableReplies :: MonadIO m => PostID -> RedditT m ()
disableReplies = nothing . runRoute . Route.sendReplies False

removePost :: MonadIO m => PostID -> RedditT m ()
removePost = nothing . runRoute . Route.removePost False

markPostAsSpam :: MonadIO m => PostID -> RedditT m ()
markPostAsSpam = nothing . runRoute . Route.removePost True
