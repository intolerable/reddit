reddit for haskell
---

A Haskell library for interacting with the [Reddit API](http://reddit.com/api). Requires [`api-builder`](https://github.com/intolerable/api-builder), but you will probably have to install it from the repository, because it needs a newer version than the one that's on Hackage.

### Basic usage

Almost everything is inside the `Reddit` monad, which handles errors and rate limiting (if you use `runRedditWithRateLimiting`) for you seamlessly – you can run the `Reddit` monad with `runReddit user pass`, which logs you in, grabs a token and starts running your actions.

Most of the time you can get away with importing `Reddit.API` and `Reddit.API.Types` and most of the stuff is available from there.

Here are some examples of functions that are useful:

```
getPostInfo :: PostID -> Reddit Post
getPosts :: SubredditName -> Reddit PostListing
savePost :: PostID -> Reddit ()
deletePost :: PostID -> Reddit ()
```

Most of the functions available have prime (`'`) versions which accept extra options (`before`, `after` and `limit` api params).

For example, `getPosts :: SubredditName -> Reddit PostListing` corresponds to `getPosts' :: Options PostID -> ListingType -> SubredditName -> Reddit PostListing`, and most other functions follow this pattern.

Testing
===

The `test` test suite will run the tests that don't rely on doing any IO, but the `test-io` should be used too to ensure that IO functions do what they're supposed to do. If you want to run the IO suite, add a file `test.cfg` to the `reddit/` directory, containing (one field per line):

- a reddit username
- a reddit password
- a subreddit name (the user should be a moderator for the subredit)

Run `cabal test test-io` and the tests should pass.