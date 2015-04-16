module Reddit.Actions.Post
  ( getPosts
  , getPosts'
  , getPostComments
  , getPostSubComments
  , Reddit.Actions.Post.getComments
  , getPostInfo
  , getPostsInfo
  , Reddit.Actions.Post.submitLink
  , Reddit.Actions.Post.submitLinkWithCaptcha
  , Reddit.Actions.Post.submitSelfPost
  , Reddit.Actions.Post.submitSelfPostWithCaptcha
  , enableReplies
  , disableReplies
  , Reddit.Actions.Post.savePost
  , Reddit.Actions.Post.unsavePost
  , Reddit.Actions.Post.editPost
  , deletePost
  , setPostFlair
  , Reddit.Actions.Post.removePost
  , markPostSpam
  , Reddit.Actions.Post.stickyPost
  , unstickyPost
  , enableContestMode
  , disableContestMode ) where

import Reddit.Routes as Route
import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Captcha
import Reddit.Types.Comment
import Reddit.Types.Empty
import Reddit.Types.Error
import Reddit.Types.Listing
import Reddit.Types.Options
import Reddit.Types.Reddit

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
    Listing _ _ [post] -> return post
    _ -> failWith $ APIError InvalidResponseError

getPostsInfo :: MonadIO m => [PostID] -> RedditT m PostListing
getPostsInfo ps =
  -- we can only get 100 posts at a time or the api shits itself
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

-- | Save a post.
savePost :: MonadIO m => PostID -> RedditT m ()
savePost = nothing . runRoute . Route.savePost

-- | Remove a saved post from your "saved posts" list.
unsavePost :: MonadIO m => PostID -> RedditT m ()
unsavePost = nothing . runRoute . Route.unsavePost

submitLink :: MonadIO m => SubredditName -> Text -> Text -> RedditT m PostID
submitLink r title url = do
  POSTWrapped res <- runRoute $ Route.submitLink r title url
  return res

submitLinkWithCaptcha :: MonadIO m => SubredditName -> Text -> Text -> CaptchaID -> Text -> RedditT m PostID
submitLinkWithCaptcha r title url iden captcha = do
  POSTWrapped res <- runRoute $ Route.submitLink r title url `withCaptcha` (iden, captcha)
  return res

submitSelfPost :: MonadIO m => SubredditName -> Text -> Text -> RedditT m PostID
submitSelfPost r title postBody = do
  POSTWrapped res <- runRoute $ Route.submitSelfPost r title postBody
  return res

submitSelfPostWithCaptcha :: MonadIO m => SubredditName -> Text -> Text -> CaptchaID -> Text -> RedditT m PostID
submitSelfPostWithCaptcha r title postBody iden captcha = do
  POSTWrapped res <- runRoute $ Route.submitSelfPost r title postBody `withCaptcha` (iden, captcha)
  return res

deletePost :: MonadIO m => PostID -> RedditT m ()
deletePost = nothing . runRoute . Route.delete

setPostFlair :: MonadIO m => SubredditName -> PostID -> Text -> Text -> RedditT m ()
setPostFlair r p text css = nothing $ runRoute $ Route.postFlair r p text css

-- | Edit the text of a self-post.
editPost :: MonadIO m => PostID -> Text -> RedditT m ()
editPost thing text = nothing $ runRoute $ Route.edit thing text

-- | Get a post and all its comments.
getPostComments :: MonadIO m => PostID -> RedditT m PostComments
getPostComments p = runRoute $ Route.getComments p Nothing

-- | Get a post and a specific sub-tree of comments.
getPostSubComments :: MonadIO m => PostID -> CommentID -> RedditT m PostComments
getPostSubComments p c = runRoute $ Route.getComments p (Just c)

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

enableContestMode :: MonadIO m => PostID -> RedditT m ()
enableContestMode = nothing . runRoute . Route.setContestMode True

disableContestMode :: MonadIO m => PostID -> RedditT m ()
disableContestMode = nothing . runRoute . Route.setContestMode False
