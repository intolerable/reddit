module Reddit.API.Routes.Thing where

import Reddit.API.Types.Thing

import APIBuilder.Routes
import Data.Text (Text)

reply :: Thing a => a -> Text -> Route
reply thingID body = Route [ "api", "comment" ]
                           [ "parent" =. Just (fullName thingID)
                           , "text" =. Just body ] 
                           "POST"

delete :: Thing a => a -> Route
delete t = Route [ "api", "del" ]
                 [ "id" =. Just (fullName t) ]
                 "POST"
