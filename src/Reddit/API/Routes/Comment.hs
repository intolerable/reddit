module Reddit.API.Routes.Comment where

import Reddit.API.Types.Comment
import Reddit.API.Types.Post
import Reddit.API.Types.Subreddit

import Network.API.Builder.Routes

aboutComment :: CommentID -> Route
aboutComment pID = Route [ "api", "info" ]
                         [ "id" =. pID ]
                         "GET"

moreChildren :: PostID -> [CommentID] -> Route
moreChildren p cs = Route [ "api", "morechildren" ]
                          [ "link_id" =. p
                          , "children" =. map (\(CommentID x) -> x) cs ]
                          "POST"

newSubredditComments :: SubredditName -> Route
newSubredditComments (R sub) = Route [ "r",  sub, "comments" ]
                                     [ ]
                                     "GET"
