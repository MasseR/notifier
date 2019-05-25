{-# LANGUAGE OverloadedStrings #-}
module Feed
  ( getFeedContent
  , Content(..)
  , parseFeed
  ) where


import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text as T
import           Text.Feed.Import     (parseFeedSource)
import           Text.Feed.Query
import           Text.Feed.Types
import Debug.Trace


data Content = Content { hash    :: Text
                       , content :: Text
                       } deriving Show

getFeedContent :: Feed -> [Content]
getFeedContent = mapMaybe getContent . getFeedItems
  where
    getContent :: Item -> Maybe Content
    getContent item = Content <$> getId item <*> (T.strip <$> getItemTitle item)
    getId = fmap (snd . traceShowId) . getItemId

parseFeed :: ByteString -> Maybe Feed
parseFeed = parseFeedSource
