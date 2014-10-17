module Reddit.API.Types.WikiSpec where

import Reddit.API.Types.Wiki

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Types.Wiki" $

  it "should correctly unescape text" $ do
    unescape "&gt;" `shouldBe` ">"
    unescape "&lt;" `shouldBe` "<"
    unescape "&amp;" `shouldBe` "&"
    unescape "&lt;br/&gt;" `shouldBe` "<br/>"
