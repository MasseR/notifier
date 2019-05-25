module Feed
  ( getFeedContent
  , Content(..)
  ) where


import           Data.Text       (Text)
import           Text.Feed.Types
import           Text.Feed.Query
import Data.Maybe (mapMaybe)


data Content = Content { hash    :: Text
                       , content :: Text
                       } deriving Show

getFeedContent :: Feed -> [Content]
getFeedContent = mapMaybe getContent . getFeedItems
  where
    getContent :: Item -> Maybe Content
    getContent item = Content <$> getId item <*> getItemSummary item
    getId = fmap snd . getItemId
