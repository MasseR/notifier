module Main where

import Feed
import Network.Wreq
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Lens

fetchFeed :: MonadIO m => String -> m [Content]
fetchFeed url = do
  resp <- view responseBody <$> liftIO (get url)
  pure (maybe [] getFeedContent (parseFeed resp))

main :: IO ()
main = putStrLn "Hello, Haskell!"
