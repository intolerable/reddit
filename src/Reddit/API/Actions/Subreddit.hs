module Reddit.API.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings
  , setSubredditSettings ) where

import Reddit.API.Types
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Types.Empty
import qualified Reddit.API.Routes as Route

import APIBuilder

getSubredditInfo :: SubredditName -> Reddit Subreddit
getSubredditInfo = RedditT . runRoute . Route.aboutSubreddit

getSubredditSettings :: SubredditName -> Reddit SubredditSettings
getSubredditSettings = RedditT . runRoute . Route.subredditSettings

setSubredditSettings :: SubredditID -> SubredditSettings -> Reddit ()
setSubredditSettings r s = nothing $ RedditT $ runRoute (Route.setSubredditSettings r s)
