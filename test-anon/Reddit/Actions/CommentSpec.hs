module Reddit.Actions.CommentSpec where

import Reddit
import Utils

import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Comment" $ do

  it "should be able to get info for a comment" $ do
    res <- runAnon $ getCommentInfo (CommentID "c60o0iw")
    res `shouldSatisfy` isRight

  it "should be able to get info for multiple comments" $ do
    res <- runAnon $ getCommentsInfo [CommentID "c60o0iw", CommentID "c4zrenn"]
    res `shouldSatisfy` isRight
    case res of
      Right (Listing _ _ cs) -> length cs `shouldBe` 2
      Left _ -> expectationFailure "something failed"

  it "should fail if we try to get an invalid comment" $ do
    res <- runAnon $ getCommentInfo (CommentID "!!!!!!!")
    res `shouldSatisfy` isLeft

  it "shouldn't be able to get a list of comment IDs where some are invalid" $ do
    res <- runAnon $ getCommentsInfo [CommentID "c60o0iw", CommentID "!!!!!!!"]
    res `shouldSatisfy` isLeft

  it "should be able to get mass comment IDs" $ do
    let a = replicate 100 $ CommentID "c60o0iw"
    res <- runAnon $ getCommentsInfo a
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) ->
        length ps `shouldBe` length a

  it "should fail if it tries to get TOO many comment IDs" $ do
    let a = replicate 101 $ CommentID "c60o0iw"
    res <- runAnon $ getCommentsInfo a
    res `shouldSatisfy` isLeft
