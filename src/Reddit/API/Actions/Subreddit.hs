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
getSubredditInfo = Reddit . runRoute . Route.aboutSubreddit

getSubredditSettings :: SubredditName -> Reddit SubredditSettings
getSubredditSettings = Reddit . runRoute . Route.subredditSettings

setSubredditSettings :: SubredditID -> SubredditSettings -> Reddit ()
setSubredditSettings r s = nothing $ Reddit $ runRoute (Route.setSubredditSettings r s)
