module Reddit.Actions.SearchSpec where

import Reddit
import Reddit.Types.SearchOptions

import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Search" $ do

  it "should be able to search for something" $ do
    res <- runRedditAnon $ search Nothing (Options Nothing Nothing) Hot "reddit"
    res `shouldSatisfy` isRight

  it "should be able to limit the search results" $  do
    res <- runRedditAnon $ search (Just $ R "programming") (Options Nothing (Just 5)) Hot "haskell"
    case res of
      Left _ -> expectationFailure "search failed"
      Right (Listing _ _ rs) -> length rs `shouldBe` 5
