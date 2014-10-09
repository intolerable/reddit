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
