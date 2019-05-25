module Feed
  ( getFeedContent
  , Content(..)
  , parseFeed
  ) where


import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Text.Feed.Import     (parseFeedSource)
import           Text.Feed.Query
import           Text.Feed.Types

type LByteString = LB.ByteString
type ByteString = B.ByteString

data Content = Content { hash    :: ByteString
                       , content :: Text
                       } deriving Show

getFeedContent :: Feed -> [Content]
getFeedContent = mapMaybe getContent . getFeedItems
  where
    getContent :: Item -> Maybe Content
    getContent item = Content <$> getId item <*> (T.strip <$> getItemTitle item)
    getId = fmap (T.encodeUtf8 . snd) . getItemId

parseFeed :: LByteString -> Maybe Feed
parseFeed = parseFeedSource
