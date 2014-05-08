module Reddit.API.Actions.Subreddit
  ( getSubredditInfo
  , getSubredditSettings ) where

import Reddit.API.Types
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes as Route

import APIBuilder

getSubredditInfo :: SubredditName -> Reddit Subreddit
getSubredditInfo = Reddit . runRoute . Route.aboutSubreddit

getSubredditSettings :: SubredditName -> Reddit SubredditSettings
getSubredditSettings = Reddit . runRoute . Route.subredditSettings
