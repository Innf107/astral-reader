module Request 
    ( request
    , RequestError (..)
    ) where

import Relude

import Text.Feed.Import qualified as Feed
import Text.Feed.Query qualified as Query

import Text.URI

import Network.HTTP.Req qualified as Req

import ReaderFeed

import Debug.Trace qualified as Trace
import Control.Exception

data RequestError
    = UrlParseError Text
    | InvalidStatusCode Int
    | FeedParseError
    | IncompleteFeed
    | RequestException SomeException
    deriving Show

requestUrl :: Req.Url scheme -> Req.Option scheme -> IO (Either RequestError ReaderFeedBuilder)
requestUrl url options = do
    response <- Req.runReq Req.defaultHttpConfig do
        Req.req
            Req.GET
            url
            Req.NoReqBody
            Req.lbsResponse
            options
    let statusCode = Req.responseStatusCode response
    if statusCode /= 200 then
        pure (Left (InvalidStatusCode statusCode))
    else do
        case Feed.parseFeedSource (Req.responseBody response) of
            Nothing -> pure (Left FeedParseError)
            Just feed -> do
                let feedTitle = Query.getFeedTitle feed

                let items = (Query.feedItems feed) & mapMaybe \item -> do
                        itemDate <- Query.getItemDate item
                        itemPublishDate <- join $ Query.getItemPublishDate item
                        itemTitle <- Query.getItemTitle item
                        itemLink <- Query.getItemLink item
                        pure ReaderFeedItem 
                            { feedTitle
                            , itemDate
                            , itemPublishDate
                            , itemTitle
                            , itemLink
                            }

                pure (Right (listToBuilder items))
        

request :: Text -> IO (Either RequestError ReaderFeedBuilder)
request source = do
    uri <- mkURI source

    result <- try $ case Req.useURI uri of
        Nothing -> pure (Left (UrlParseError source))
        Just (Left  (url, options)) -> requestUrl url options
        Just (Right (url, options)) -> requestUrl url options

    Trace.traceIO ("Collection of " <> toString source <> " completed!")

    case result of
        Left exn -> do
            Trace.traceIO ("\ESC[31mERROR: " <> show exn <> "\ESC[0m")
            pure (Left (RequestException exn))
        Right result -> pure result



