module Reddit.API.Routes.Comment where

import Reddit.API.Types.Comment (CommentID)

import APIBuilder.Routes

aboutComment :: CommentID -> Route
aboutComment pID = Route [ "api", "info" ]
                         [ "id" =. pID ]
                         "GET"
