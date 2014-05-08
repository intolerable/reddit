module Reddit.API.Routes.Post where

import Reddit.API.Types.Post (PostID(..))
import Reddit.API.Types.Subreddit (SubredditName(..))
import Reddit.API.Types.Thing

import APIBuilder.Routes
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T

postsListing :: Maybe SubredditName -> Text -> Route
postsListing r t = Route (endpoint r)
                         ["limit" =. Just "50"]
                         GET
  where endpoint Nothing = [ t ]
        endpoint (Just (R name)) = [ "r", name, t ]

aboutPost :: PostID -> Route
aboutPost p = Route [ "api", "info" ]
                    [ "id" =. (Just $ fullName p) ]
                    GET 

savePost :: PostID -> Route
savePost p = Route [ "api", "save" ]
                   [ "id" =. (Just $ fullName p) ] 
                   POST

unsavePost :: PostID -> Route
unsavePost p = Route [ "api", "unsave" ]
                     [ "id" =. (Just $ fullName p) ]
                     POST

submitLink :: SubredditName -> Text -> Text -> Route
submitLink (R name) title url = Route [ "api", "submit" ]
                                      [ "extension" =. Just "json"
                                      , "kind" =. Just "link"
                                      , "save" =. Just "false"
                                      , "resubmit" =. Just "false"
                                      , "sendreplies" =. Just "true"
                                      , "then" =. Just "tb"
                                      , "title" =. Just title
                                      , "url" =. Just url
                                      , "sr" =. Just name]
                                      POST

deletePost :: PostID -> Route
deletePost p = Route [ "api", "del" ]
                     [ "id" =. Just (fullName p) ]
                     POST

getComments :: PostID -> Route
getComments (PostID p) = Route [ "comments", p ]
                               [ ]
                               GET

sendReplies :: Bool -> PostID -> Route
sendReplies setting p = Route [ "api", "sendreplies" ]
                              [ "id" =. Just (fullName p)
                              , "state" =. Just (T.pack $ map toLower $ show setting) ]
                              POST
