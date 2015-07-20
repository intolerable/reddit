module Reddit.Actions.CommentSpec where

import Reddit.Actions.Comment
import Reddit.Types.Comment
import Reddit.Types.Listing

import ConfigLoad
import Control.Monad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Comment" $ do
  (reddit, anon, _, _) <- runIO loadConfig

  it "should be able to get info for a comment" $ do
    forM_ [reddit, anon] $ \acc -> do
      res <- run acc $ getCommentInfo (CommentID "c60o0iw")
      res `shouldSatisfy` isRight

  it "should be able to get info for multiple comments" $ do
    forM_ [reddit, anon] $ \acc -> do
      res <- run acc $ getCommentsInfo [CommentID "c60o0iw", CommentID "c4zrenn"]
      res `shouldSatisfy` isRight
      case res of
        Right (Listing _ _ cs) -> length cs `shouldBe` 2
        Left _ -> expectationFailure "something failed"

  it "should fail if we try to get an invalid comment" $ do
    forM_ [reddit, anon] $ \acc -> do
      res <- run acc $ getCommentInfo (CommentID "garbage")
      res `shouldSatisfy` isLeft

  it "shouldn't be able to get a list of comment IDs where some are invalid" $ do
    forM_ [reddit, anon] $ \acc -> do
      res <- run acc $ getCommentsInfo [CommentID "c60o0iw", CommentID "garbage"]
      res `shouldSatisfy` isLeft

  it "should be able to get mass comment IDs" $ do
    forM_ [reddit, anon] $ \acc -> do
      let a = replicate 100 $ CommentID "c60o0iw"
      res <- run acc $ getCommentsInfo a
      res `shouldSatisfy` isRight
      case res of
        Left _ -> expectationFailure "something failed"
        Right (Listing _ _ ps) ->
          length ps `shouldBe` length a

  it "should fail if it tries to get TOO many comment IDs" $ do
    forM_ [reddit, anon] $ \acc -> do
      let a = replicate 101 $ CommentID "c60o0iw"
      res <- run acc $ getCommentsInfo a
      res `shouldSatisfy` isLeft
