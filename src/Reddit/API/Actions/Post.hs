module Reddit.API.Actions.Post where

import Reddit.API.Routes as Route
import Reddit.API.Routes.Run
import Reddit.API.Types
import Reddit.API.Types.Comment
import Reddit.API.Types.Empty
import Reddit.API.Types.Error
import Reddit.API.Types.Listing
import Reddit.API.Types.Options
import Reddit.API.Types.Reddit

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Network.API.Builder.Error (APIError(..))
import qualified Data.Char as Char
import qualified Data.Text as Text

getPostInfo :: MonadIO m => PostID -> RedditT m Post
getPostInfo = getPostInfo' def

getPostInfo' :: MonadIO m => Options PostID -> PostID -> RedditT m Post
getPostInfo' opts pID = do
  res <- getPostsInfo' opts [pID]
  case res of
    Listing _ _ (p:[]) -> return p
    _ -> failWith $ APIError InvalidResponseError

getPostsInfo :: MonadIO m => [PostID] -> RedditT m PostListing
getPostsInfo = getPostsInfo' def

getPostsInfo' :: MonadIO m => Options PostID -> [PostID] -> RedditT m PostListing
getPostsInfo' opts ps = runRoute $ Route.aboutPosts opts ps

getPosts :: MonadIO m => RedditT m PostListing
getPosts = getPosts' def Hot Nothing

getPosts' :: MonadIO m => Options PostID -> ListingType -> Maybe SubredditName -> RedditT m PostListing
getPosts' o l r = runRoute $ Route.postsListing o r (Text.pack $ lower $ show l)
  where lower = map Char.toLower

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
