module Reddit.API.Routes.Post where

import Reddit.API.Types.Thing
import Reddit.API.Types.Post (PostID(..))
import Reddit.API.Types.Subreddit (SubredditName(..))

import APIBuilder.Query
import APIBuilder.Routes
import Data.Text (Text)

postsListing :: Maybe SubredditName -> Text -> Route
postsListing r t = Route (endpoint r)
                         ["limit" =. (50 :: Integer)]
                         "GET"
  where endpoint Nothing = [ t ]
        endpoint (Just (R name)) = [ "r", name, t ]

aboutPost :: PostID -> Route
aboutPost p = Route [ "api", "info" ]
                    [ "id" =. p ]
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
