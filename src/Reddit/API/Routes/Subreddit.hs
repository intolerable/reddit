module Reddit.API.Routes.Subreddit where

import Reddit.API.Types.Subreddit

import APIBuilder.Routes

aboutSubreddit :: SubredditName -> Route
aboutSubreddit (R sub) = Route ["r", sub, "about"]
                               []
                               "GET"

subredditSettings :: SubredditName -> Route
subredditSettings (R sub) = Route ["r", sub, "about", "edit"]
                                  []
                                  "GET"
