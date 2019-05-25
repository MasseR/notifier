{-# LANGUAGE RecordWildCards #-}
module Main where

import           App
import           Config
import           Control.Lens
import           Control.Monad        ((<=<))
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

notifyItem :: (MonadIO m, WithDB env m, WithNotify env m) => Content -> m ()
notifyItem Content{..} = forUnseen hash (notify message)
  where
    message :: Message
    message = Message (T.unpack content)

defaultMain :: ReaderT App IO ()
defaultMain = do
  setup
  urls <- asks (feeds . config)
  mapM_ notifyFeed urls
  where
    notifyFeed = mapM_ notifyItem <=< fetchFeed

main :: IO ()
main = do
  base <- getXdgDirectory XdgData "notifier"
  putStrLn base
  createDirectoryIfMissing True base

  conf <- loadConfig

  withSession $ \n ->
    withConnection (base </> "state.sql") $ \c ->
      runReaderT defaultMain (App n c conf)
