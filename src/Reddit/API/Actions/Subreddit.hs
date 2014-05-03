module Reddit.API.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings ) where

import Reddit.API.Types
import qualified Reddit.API.Routes as Route

import APIBuilder

getSubredditInfo :: SubredditName -> Reddit Subreddit
getSubredditInfo = runRoute . Route.aboutSubreddit

getSubredditSettings :: SubredditName -> Reddit SubredditSettings
getSubredditSettings = runRoute . Route.subredditSettings