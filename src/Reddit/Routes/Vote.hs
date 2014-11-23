module Reddit.Routes.Vote where

import Reddit.Types.Thing

import Network.API.Builder.Routes
import qualified Data.Text as T

vote :: Thing a => Int -> a -> Route
vote direction tID = Route [ "api", "vote" ]
                           [ "id" =. Just (fullName tID)
                           , "dir" =. Just (T.pack $ show direction) ]
                           "POST"
