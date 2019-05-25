{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Lens
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Text           as T
import           Feed
import           Network.Wreq
import           Notify

fetchFeed :: MonadIO m => String -> m [Content]
fetchFeed url = do
  resp <- view responseBody <$> liftIO (get url)
  pure (maybe [] getFeedContent (parseFeed resp))

notifyItem :: (MonadIO m, WithNotify env m) => Content -> m ()
notifyItem item = notify (toMessage item)
  where
    toMessage :: Content -> Message
    toMessage Content{content} = Message (T.unpack content)

main :: IO ()
main = putStrLn "Hello, Haskell!"
