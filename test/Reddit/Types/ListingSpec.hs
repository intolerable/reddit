module Reddit.Types.ListingSpec where

import Reddit.Types.Comment
import Reddit.Types.Listing

import Control.Monad
import Data.Either
import Network.API.Builder
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Listing" $ do
  describe "Listing" $ do
    getUserCommentsExample <- runIO $ ByteString.readFile "test/data/getUserComments_example.json"

    it "can read the example" $
      getUserCommentsExample `shouldSatisfy` not . ByteString.null

    it "can parse a listing from json" $ do
      let decoded = eitherDecode getUserCommentsExample :: Either String CommentListing
      decoded `shouldSatisfy` isRight

      case decoded of
        Left _ -> expectationFailure "json parse failed"
        Right (Listing b a _) -> do
          a `shouldBe` Just (CommentID "cl1royq")
          b `shouldBe` Nothing

    describe "has a valid Functor instance" $ do
      prop "length l = length (fmap f l)" $ \xs ->
        let l = Listing Nothing Nothing (xs :: [()]) in
        length (contents (l :: Listing () ())) == length (contents (void l))

      prop "fmap id x == x" $ \xs ->
        let l = Listing Nothing Nothing (xs :: [String]) in
        fmap id l == (l :: Listing () String)

    it "can parse listings from empty strings" $ do
      let decoded :: Either String CommentListing
          decoded = eitherDecode "\"\""
      decoded `shouldBe` Right (Listing Nothing Nothing [])

    it "can parse listings from null" $ do
      let decoded :: Either String CommentListing
          decoded = eitherDecode "null"
      decoded `shouldBe` Right (Listing Nothing Nothing [])

  describe "ListingType" $

    it "should have a valid ToQuery instance" $ do
      toQuery "type" Hot `shouldBe` [("type", "hot")]
      toQuery "type" New `shouldBe` [("type", "new")]
      toQuery "type" Rising `shouldBe` [("type", "rising")]
      toQuery "type" Controversial `shouldBe` [("type", "controversial")]
      toQuery "type" Top `shouldBe` [("type", "top")]
