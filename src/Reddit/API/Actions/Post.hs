module Reddit.API.Actions.Post
  ( getPosts
  , getPosts'
  , getPostComments
  , Reddit.API.Actions.Post.getComments
  , getPostInfo
  , getPostsInfo
  , Reddit.API.Actions.Post.submitLink
  , Reddit.API.Actions.Post.submitSelfPost
  , enableReplies
  , disableReplies
  , Reddit.API.Actions.Post.savePost
  , Reddit.API.Actions.Post.unsavePost
  , Reddit.API.Actions.Post.editPost
  , deletePost
  , setPostFlair
  , Reddit.API.Actions.Post.removePost
  , markPostSpam
  , Reddit.API.Actions.Post.stickyPost
  , unstickyPost ) where

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
getPostInfo p = do
  res <- getPostsInfo [p]
  case res of
    Listing _ _ (post:[]) -> return post
    _ -> failWith $ APIError InvalidResponseError

getPostsInfo :: MonadIO m => [PostID] -> RedditT m PostListing
getPostsInfo ps =
  if null $ drop 100 ps
    then do
      res <- runRoute $ Route.aboutPosts ps
      case res of
        Listing _ _ posts | sameLength posts ps ->
          return res
        _ -> failWith $ APIError InvalidResponseError
    else failWith $ APIError TooManyRequests
  where
    sameLength (_:xs) (_:ys) = sameLength xs ys
    sameLength [] [] = True
    sameLength _ _ = False

-- | Get a 'PostListing' for the 'Hot' posts on the site overall.
--   This maps to <http://reddit.com>.
getPosts :: MonadIO m => RedditT m PostListing
getPosts = getPosts' def Hot Nothing

-- | Get a 'PostListing' for a specified listing.
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

submitSelfPost :: MonadIO m => SubredditName -> Text -> Text -> RedditT m PostID
submitSelfPost r title postBody = do
  POSTWrapped res <- runRoute $ Route.submitSelfPost r title postBody
  return res

deletePost :: MonadIO m => PostID -> RedditT m ()
deletePost = nothing . runRoute . Route.delete

setPostFlair :: MonadIO m => SubredditName -> PostID -> Text -> Text -> RedditT m ()
setPostFlair r p text css = nothing $ runRoute $ Route.postFlair r p text css

editPost :: MonadIO m => PostID -> Text -> RedditT m ()
editPost thing text = nothing $ runRoute $ Route.edit thing text

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

markPostSpam :: MonadIO m => PostID -> RedditT m ()
markPostSpam = nothing . runRoute . Route.removePost True

stickyPost :: MonadIO m => PostID -> RedditT m ()
stickyPost = nothing . runRoute . Route.stickyPost True

unstickyPost :: MonadIO m => PostID -> RedditT m ()
unstickyPost = nothing . runRoute . Route.stickyPost False

