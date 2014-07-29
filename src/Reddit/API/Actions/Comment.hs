module Reddit.API.Actions.Comment where

import Reddit.API.Types.Comment
import Reddit.API.Types.Post
import Reddit.API.Types.Reddit
import Reddit.API.Types.Subreddit
import Reddit.API.Routes.Run
import qualified Reddit.API.Routes as Route

getMoreChildren :: PostID -> [CommentID] -> Reddit [CommentReference]
getMoreChildren _ [] = return []
getMoreChildren p cs = do
  let (now, next) = splitAt 20 cs
  POSTWrapped rs <- runRoute $ Route.moreChildren p now
  more <- getMoreChildren p next
  return $ rs ++ more

getNewSubredditComments :: SubredditName -> Reddit [CommentReference]
getNewSubredditComments = runRoute . Route.newSubredditComments
