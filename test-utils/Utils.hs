module Utils where

import Reddit
import Data.Default.Class

runAnon :: Reddit a -> IO (Either (APIError RedditError) a)
runAnon =
  runRedditWith def { customUserAgent = Just "reddit haskell test suite" }