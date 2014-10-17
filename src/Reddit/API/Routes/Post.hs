module Reddit.API.Routes.Post where

import Reddit.API.Types.Options
import Reddit.API.Types.Post (PostID(..))
import Reddit.API.Types.Subreddit (SubredditName(..))
import Reddit.API.Types.Thing

import Data.Text (Text)
import Network.API.Builder.Query
import Network.API.Builder.Routes

postsListing :: Options PostID -> Maybe SubredditName -> Text -> Route
postsListing opts r t =
  Route (endpoint r)
        [ "before" =. before opts
        , "after" =. after opts
        , "limit" =. limit opts ]
        "GET"
  where endpoint Nothing = [ t ]
        endpoint (Just (R name)) = [ "r", name, t ]

aboutPosts :: [PostID] -> Route
aboutPosts ps = Route [ "api", "info" ]
                      [ "id" =. ps ]
                      "GET"

savePost :: PostID -> Route
savePost p = Route [ "api", "save" ]
                   [ "id" =. p ]
                   "POST"

unsavePost :: PostID -> Route
unsavePost p = Route [ "api", "unsave" ]
                     [ "id" =. p ]
                     "POST"

submitLink :: SubredditName -> Text -> Text -> Route
submitLink (R name) title url = Route [ "api", "submit" ]
                                      [ "extension" =. ("json" :: Text)
                                      , "kind" =. ("link" :: Text)
                                      , "save" =. True
                                      , "resubmit" =. False
                                      , "sendreplies" =. True
                                      , "then" =. ("tb" :: Text)
                                      , "title" =. title
                                      , "url" =. url
                                      , "sr" =. name]
                                      "POST"

deletePost :: PostID -> Route
deletePost p = Route [ "api", "del" ]
                     [ "id" =. p ]
                     "POST"

getComments :: PostID -> Route
getComments (PostID p) = Route [ "comments", p ]
                               [ ]
                               "GET"

sendReplies :: Bool -> PostID -> Route
sendReplies setting p = Route [ "api", "sendreplies" ]
                              [ "id" =. p
                              , "state" =. setting ]
                              "POST"

removePost :: (ToQuery a, Thing a) => Bool -> a -> Route
removePost isSpam p = Route [ "api", "remove" ]
                            [ "id" =. p
                            , "spam" =. isSpam ]
                            "POST"
