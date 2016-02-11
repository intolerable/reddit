{-# LANGUAGE OverloadedStrings #-}
-- |

module Main where

import           Reddit
import           Reddit.Types.Post
import           Reddit.Types.User

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import           Data.Ord
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

main :: IO ()
main = do
  _ <- runRedditAnon $ do
    Listing _ _ posts <- getPosts
    forM_ posts $ \post -> do
      liftIO $ putStrLn $
        "[" <> show (score post) <> "] " <>
        (show $ title post) <> " (" <> show (subreddit post) <> ")"

    infos <- mapM (getUserInfo . Username) usersToCheck
    liftIO $ print $ maximumBy (comparing linkKarma) infos
  return ()


usersToCheck :: [Text.Text]
usersToCheck = ["nikita-volkov", "simonmar", "bos", "roche"]

tshow :: Show a => a -> Text.Text
tshow = Text.pack . show
