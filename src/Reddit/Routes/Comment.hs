module Reddit.Routes.Comment where

import Reddit.Types.Comment
import Reddit.Types.Options
import Reddit.Types.Post
import Reddit.Types.Subreddit

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

newComments :: Options CommentID -> Maybe SubredditName -> Route
newComments opts r =
  Route url
        [ "before" =. before opts
        , "after" =. after opts
        , "limit" =. limit opts ]
        "GET"
  where
    url = case r of
      Just (R sub) -> [ "r", sub, "comments" ]
      Nothing -> [ "comments" ]

commentsInfo :: [CommentID] -> Route
commentsInfo cs =
  Route [ "api", "info" ]
        [ "id" =. cs ]
        "GET"
