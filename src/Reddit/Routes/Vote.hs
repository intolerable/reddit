module Reddit.Routes.Vote where

import Reddit.Types.Thing

import Network.API.Builder.Query
import Network.API.Builder.Routes

data VoteDirection = UpVote
                   | RemoveVote
                   | DownVote
  deriving (Show, Read, Eq)

instance ToQuery VoteDirection where
  toQuery k UpVote = [(k, "1")]
  toQuery k RemoveVote = [(k, "0")]
  toQuery k DownVote = [(k, "-1")]

vote :: Thing a => VoteDirection -> a -> Route
vote direction tID = Route [ "api", "vote" ]
                           [ "id" =. Just (fullName tID)
                           , "dir" =. direction ]
                           "POST"
