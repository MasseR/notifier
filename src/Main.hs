{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           App
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans  (MonadIO, liftIO)
import qualified Data.Text            as T
import           DB
import           Feed
import           Network.Wreq
import           Notify
import           System.Directory
import           System.FilePath      ((</>))

fetchFeed :: MonadIO m => String -> m [Content]
fetchFeed url = do
  resp <- view responseBody <$> liftIO (get url)
  pure (maybe [] getFeedContent (parseFeed resp))

notifyItem :: (MonadIO m, WithNotify env m) => Content -> m ()
notifyItem item = notify (toMessage item)
  where
    toMessage :: Content -> Message
    toMessage Content{content} = Message (T.unpack content)

defaultMain :: ReaderT App IO ()
defaultMain = do
  liftIO $ putStrLn "Setting up"
  setup

main :: IO ()
main = do
  base <- getXdgDirectory XdgData "notifier"
  putStrLn base
  createDirectoryIfMissing True base

  withSession $ \n ->
    withConnection (base </> "state.sql") $ \c ->
      runReaderT defaultMain (App n c)
