module ReaderFeed 
    ( ReaderFeedBuilder
    , ReaderFeedItem (..)
    , buildFeed
    , listToBuilder
    ) where
import Relude

import Text.RSS.Syntax (DateString)
import Data.Time (UTCTime)

data ReaderFeedItem = 
    ReaderFeedItem 
    { itemDate :: DateString
    , itemPublishDate :: UTCTime
    , itemTitle :: Text
    , itemLink :: Text
    , feedTitle :: Text
    } deriving (Show)

newtype ReaderFeedBuilder = ReaderFeedBuilder {
    buildFeedRaw :: [ReaderFeedItem] -> [ReaderFeedItem]
}

instance Semigroup ReaderFeedBuilder where
    (ReaderFeedBuilder left) <> (ReaderFeedBuilder right) =
        ReaderFeedBuilder (left . right)

instance Monoid ReaderFeedBuilder where
    mempty = ReaderFeedBuilder id


buildFeed :: ReaderFeedBuilder -> [ReaderFeedItem]
buildFeed builder = do 
    let entries = buildFeedRaw builder []
    sortOn (Down . itemPublishDate) entries

listToBuilder :: [ReaderFeedItem] -> ReaderFeedBuilder
listToBuilder items = ReaderFeedBuilder \rest -> items <> rest
