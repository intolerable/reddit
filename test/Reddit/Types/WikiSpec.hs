module Reddit.Types.WikiSpec where

import Reddit.Types.Wiki

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Wiki" $

  it "should correctly unescape text" $ do
    unescape "&gt;" `shouldBe` ">"
    unescape "&lt;" `shouldBe` "<"
    unescape "&amp;" `shouldBe` "&"
    unescape "&lt;br/&gt;" `shouldBe` "<br/>"
