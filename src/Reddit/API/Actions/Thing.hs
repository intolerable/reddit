module Reddit.API.Actions.Thing where

import Reddit.API.Types
import Reddit.API.Types.Empty
import Reddit.API.Types.Reddit
import qualified Reddit.API.Routes.Thing as Route

import APIBuilder
import Data.Text (Text)

reply :: Thing a => a -> Text -> Reddit CommentID
reply t b = do
  POSTWrapped res <- runRoute $ Route.reply t b
  return $ res

delete :: Thing a => a -> Reddit ()
delete = nothing . runRoute . Route.delete