module Reddit.API.Routes.Comment where

import Reddit.API.Routes.Thing
import Reddit.API.Types.Comment (CommentID)
import Reddit.API.Types.Thing

import APIBuilder.Routes
import Data.Text (Text)

aboutComment :: CommentID -> Route
aboutComment pID = Route [ "api", "info" ]
                         [ "id" =. pID ]
                         "GET"
