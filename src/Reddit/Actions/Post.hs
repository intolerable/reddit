-- | Contains post-related actions, like submitting a post, getting
--   information for an existing post, and performing moderator actions
--   on posts.
module Reddit.Actions.Post
  ( getPosts
  , getPosts'
  , getPostComments
  , getPostSubComments
  , getComments
  , getPostInfo
  , getPostsInfo
  , submitLink
  , submitLinkWithCaptcha
  , submitSelfPost
  , submitSelfPostWithCaptcha
  , setInboxReplies
  , savePost
  , unsavePost
  , editPost
  , deletePost
  , setPostFlair
  , removePost
  , markPostSpam
  , stickyPost
  , unstickyPost
  , setContestMode ) where

import qualified Reddit.Routes as Route
import Reddit.Routes.Run
import Reddit.Types
import Reddit.Types.Captcha
import Reddit.Types.Comment
import Reddit.Types.Empty
import Reddit.Types.Listing
import Reddit.Types.Reddit

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
import Network.API.Builder.Error (APIError(..))
import qualified Data.Char as Char
import qualified Data.Text as Text

-- | Given a 'PostID', 'getPostInfo' will return the full details for that post.
getPostInfo :: MonadIO m => PostID -> RedditT m Post
getPostInfo p = do
  res <- getPostsInfo [p]
  case res of
    Listing _ _ [post] -> return post
    _ -> failWith $ APIError InvalidResponseError

-- | Given a list of 'PostID's, 'getPostsInfo' will return another list containing
--   the full details for all the posts. Note that Reddit's
--   API imposes a limitation of 100 posts per request, so this function will fail immediately if given a list of more than 100 IDs.
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

-- | Submit a new link to Reddit.
submitLink :: MonadIO m
           => SubredditName -- ^ The subreddit to which you're posting the link
           -> Text -- ^ The title of the link post
           -> Text -- ^ The link that you're posting
           -> RedditT m PostID
submitLink r title url = do
  POSTWrapped res <- runRoute $ Route.submitLink r title url
  return res

-- | Submit a new link to Reddit (answering a Captcha to prove we aren't a robot).
submitLinkWithCaptcha :: MonadIO m
                      => SubredditName -- ^ The subreddit to which you're posting the link
                      -> Text -- ^ The title of the link post
                      -> Text -- ^ The link that you're posting
                      -> CaptchaID -- ^ The ID of the captcha we're answering
                      -> Text -- ^ The answer to the provided captcha
                      -> RedditT m PostID
submitLinkWithCaptcha r title url iden captcha = do
  POSTWrapped res <- runRoute $ Route.submitLink r title url `withCaptcha` (iden, captcha)
  return res

-- | Submit a new selfpost to Reddit.
submitSelfPost :: MonadIO m
               => SubredditName -- ^ The subreddit to which you're posting the selfpost
               -> Text -- ^ The title of the selfpost
               -> Text -- ^ The body of the selfpost
               -> RedditT m PostID
submitSelfPost r title postBody = do
  POSTWrapped res <- runRoute $ Route.submitSelfPost r title postBody
  return res

-- | Submit a new selfpost to Reddit (answering a Captcha to prove we aren't a robot).
submitSelfPostWithCaptcha :: MonadIO m
                          => SubredditName -- ^ The subreddit to which you're posting the selfpost
                          -> Text -- ^ The title of the selfpost
                          -> Text -- ^ The body of the selfpost
                          -> CaptchaID -- ^ The ID of the captcha we're answering
                          -> Text -- ^ The answer to the provided captcha
                          -> RedditT m PostID
submitSelfPostWithCaptcha r title postBody iden captcha = do
  POSTWrapped res <- runRoute $ Route.submitSelfPost r title postBody `withCaptcha` (iden, captcha)
  return res

-- | Deletes one of your own posts. Note that this is different from
--   removing a post as a moderator action.
deletePost :: MonadIO m => PostID -> RedditT m ()
deletePost = nothing . runRoute . Route.delete

-- | Set the link flair for a post you've submitted (or any post on a subreddit
--   that you moderate).
setPostFlair :: MonadIO m
             => SubredditName -- ^ The subreddit on which to set the flair
             -> PostID -- ^ The post whose flair should be set
             -> Text -- ^ The text label for the post's new flair
             -> Text -- ^ The CSS class for the post's new flair
             -> RedditT m ()
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

-- | Get the comments for a post. Ignore the actual post itself.
getComments :: MonadIO m => PostID -> RedditT m [CommentReference]
getComments p = do
  PostComments _ c <- getPostComments p
  return c

-- | Set the state of inbox replies for the specified thread.
setInboxReplies :: MonadIO m => Bool -> PostID -> RedditT m ()
setInboxReplies enabled = nothing . runRoute . Route.sendReplies enabled

-- | Set the state of contest for the specified thread as a moderator action.
setContestMode :: MonadIO m => Bool -> PostID -> RedditT m ()
setContestMode enabled = nothing . runRoute . Route.setContestMode enabled

-- | Removes a post (as a moderator action). Note that this is different
--   from deleting a post.
removePost :: MonadIO m => PostID -> RedditT m ()
removePost = nothing . runRoute . Route.removePost False

-- | Mark a post as spam as a moderator action.
markPostSpam :: MonadIO m => PostID -> RedditT m ()
markPostSpam = nothing . runRoute . Route.removePost True

-- | Sticky a post on the subreddit on which it's posted.
stickyPost :: MonadIO m
           => PostID -- ^ The post to be stickied
           -> Maybe Integer -- ^ The position to which it should be stickied
           -> RedditT m ()
stickyPost p n = nothing $ runRoute $ Route.stickyPost True p n

-- | Unsticky a post from the subreddit on which it's posted.
unstickyPost :: MonadIO m
           => PostID -- ^ The post to be unstickied
           -> Maybe Integer -- ^ The position from which it should be unstickied
           -> RedditT m ()
unstickyPost p n = nothing $ runRoute $ Route.stickyPost False p n
