module Reddit.API.Types.ListingSpec where

import Reddit.API.Types.Comment
import Reddit.API.Types.Listing

import Control.Monad
import Data.Either
import Network.API.Builder
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Types.Listing" $ do
  getUserCommentsExample <- runIO $ ByteString.readFile "test/data/getUserComments_example.json"

  it "can read the example" $
    getUserCommentsExample `shouldSatisfy` not . ByteString.null

  it "can parse a listing from json" $ do
    let decoded = decode getUserCommentsExample :: Either (APIError ()) CommentListing
    decoded `shouldSatisfy` isRight

    case decoded of
      Left _ -> expectationFailure "json parse failed"
      Right l@(Listing b a cs) -> do
        a `shouldBe` Just (CommentID "t1_cl1royq")
        b `shouldBe` Nothing
        length cs `shouldBe` length (contents (void l))

  it "can parse listings from empty strings" $ do
    let decoded :: Either (APIError ()) CommentListing
        decoded = decode "\"\""
    decoded `shouldBe` Right (Listing Nothing Nothing [])

  it "can parse listings from null" $ do
    let decoded :: Either (APIError ()) CommentListing
        decoded = decode "null"
    decoded `shouldBe` Right (Listing Nothing Nothing [])
