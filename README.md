reddit for haskell [![Build Status](https://travis-ci.org/intolerable/reddit.svg?branch=master)](https://travis-ci.org/intolerable/reddit)
---

A Haskell library for interacting with the [Reddit API](http://reddit.com/api).

### A couple of examples

Let's get all the posts from the frontpage of Reddit and write a summary of each of them to the console:
Note that you may need the `OverloadedStrings` language pragma to get these examples to work. (Add 
```haskell
{-# LANGUAGE OverloadedStrings #-}
``` 
to your scripts, if needed.) 


```haskell
import Reddit
import Reddit.Types.Post

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main = runRedditAnon $ do
  Listing _ _ posts <- getPosts
  forM_ posts $ \post -> do
    liftIO $ Text.putStrLn $
       "[" <> tshow (score post) <> "] " <>
       title post <> " (" <> tshow (subreddit post) <> ")"

tshow = Text.pack . show
```

Let's check to see which of a group of users has the highest link karma:

```haskell
import Reddit
import Reddit.Types.User

import Data.List
import Data.Ord

usersToCheck = ["nikita-volkov", "simonmar", "bos", "roche"]

main = runRedditAnon $ do
  infos <- mapM (getUserInfo . Username) usersToCheck
  return $ maximumBy (comparing linkKarma) infos
```

Testing
===

Pure tests
---

`cabal test test`

This suite will only run test that don't require doing any IO. Helpful because it runs quickly and isn't subject to any network problems.

Anonymous tests
---

`cabal test test-anon`

There's also a suite of tests that can be run anonymously without having to set up a user account and an empty subreddit.

Full IO tests
---

`cabal test test-io`

The `test` test suite will run the tests that don't rely on doing any IO, but the `test-io` should be used too to ensure that IO functions do what they're supposed to do. If you want to run the IO suite, add a file `test.cfg` to the `reddit/` directory, containing (one field per line):

- a reddit username
- a reddit password
- a subreddit name (the user should be a moderator for the subredit)
