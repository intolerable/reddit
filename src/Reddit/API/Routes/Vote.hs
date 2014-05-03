module Reddit.API.Routes.Vote where

import Reddit.API.Types.Thing

import APIBuilder.Routes
import qualified Data.Text as T

vote :: Thing a => Int -> a -> Route
vote direction tID = Route [ "api", "vote" ]
                           [ "id" =. Just (fullName tID)
                           , "dir" =. Just (T.pack $ show direction) ]
                           POST