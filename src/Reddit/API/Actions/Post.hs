module Reddit.API.Actions.Post where

import Reddit.API.Routes as Route
import Reddit.API.Types
import Reddit.API.Types.Comment
import Reddit.API.Types.Reddit
import Reddit.API.Types.Listing
import Reddit.API.Types.Empty

import APIBuilder
import Data.Text (Text)

getPostInfo :: PostID -> Reddit Post
getPostInfo pID = do
  Listing (p:_) <- Reddit $ runRoute $ Route.aboutPost pID
  return p

getHotPosts :: Reddit (Listing Post)
getHotPosts = Reddit $ runRoute $ Route.postsListing Nothing "hot"

getHotSubredditPosts :: SubredditName -> Reddit (Listing Post)
getHotSubredditPosts r = Reddit $ runRoute $ Route.postsListing (Just r) "hot"

getNewPosts :: Reddit (Listing Post)
getNewPosts = Reddit $ runRoute $ Route.postsListing Nothing "new"

getNewSubredditPosts :: SubredditName -> Reddit (Listing Post)
getNewSubredditPosts r = Reddit $ runRoute $ Route.postsListing (Just r) "new"

savePost :: PostID -> Reddit ()
savePost = nothing . Reddit . runRoute . Route.savePost

unsavePost :: PostID -> Reddit ()
unsavePost = nothing . Reddit . runRoute . Route.unsavePost

submitLink :: SubredditName -> Text -> Text -> Reddit PostID
submitLink r title url = do
  POSTWrapped res <- Reddit $ runRoute $ Route.submitLink r title url
  return res

deletePost :: PostID -> Reddit ()
deletePost = nothing . Reddit . runRoute . Route.deletePost

getComments :: PostID -> Reddit [Comment]
getComments p = do
  PostComments _ c <- Reddit $ runRoute $ Route.getComments p
  return c

enableReplies :: PostID -> Reddit ()
enableReplies = nothing . Reddit . runRoute . Route.sendReplies True

disableReplies :: PostID -> Reddit ()
disableReplies = nothing . Reddit . runRoute . Route.sendReplies False
