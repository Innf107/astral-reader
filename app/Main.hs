module Main (main) where

import Relude

import Request qualified

import Witherable
import ReaderFeed
import System.IO (hPutStrLn)
import System.Directory (createDirectoryIfMissing)

import Web.Scotty as Scotty

import Data.Text.Lazy qualified as LText
import Control.Concurrent.Async (mapConcurrently)

logErrors :: (Text, Either Request.RequestError a) -> IO (Maybe a)
logErrors (source, Left err) = do
    hPutStrLn stderr ("\ESC[31mError reading source '" <> toString source <> "': " <> show err <> "\ESC[0m")
    pure Nothing
logErrors (_, Right feed) = pure $ Just feed

collectFeed :: [Text] -> IO ([ReaderFeedItem])
collectFeed sourceURLs = do
    putStrLn "Collecting feeds..."
    sourcesOrErrors <- mapConcurrently (\source -> (source,) <$> Request.request (toText source)) sourceURLs
    putStrLn "Done!"
    feeds <- wither logErrors sourcesOrErrors
    pure $ buildFeed $ fold feeds


applyFeedItem :: LText -> ReaderFeedItem -> LText
applyFeedItem template item = do
    let ReaderFeedItem 
            { itemDate
            , itemTitle
            , itemLink
            , feedTitle
            } = item
    foldr (\(key, value) r -> LText.replace key value r) template
        [ ("<<itemDate>>", toLazy itemDate)
        , ("<<itemTitle>>", toLazy itemTitle)
        , ("<<itemLink>>", toLazy itemLink)
        , ("<<feedTitle>>", toLazy feedTitle)
        , ("<<feedHue>>", toLazy $ hueFromFeedTitle feedTitle)
        ]

hueFromFeedTitle :: Text -> Text
hueFromFeedTitle title = do
    show $ sum (map fromEnum $ toString title) `mod` 360
    
generateHtml :: IO LText
generateHtml = do
    sourceURLs <- lines . decodeUtf8 <$> readFileLBS "sources.txt"
    feed <- collectFeed sourceURLs    

    templateHtml <- decodeUtf8 <$> readFileLBS "html/template.html"
    entryHtml <- decodeUtf8 <$> readFileLBS "html/entry.html"

    let entries = LText.concat (map (applyFeedItem entryHtml) feed)

    pure (LText.replace "<<entries>>" entries templateHtml)


main :: IO ()
main = do
    createDirectoryIfMissing False ".build"

    feedHtml <- generateHtml
    writeFileLText ".build/feed.html" feedHtml

    scotty 3000 do
        Scotty.get "/" do
            file ".build/feed.html"
        Scotty.get "/static/:path" do
            path <- param "path"
            file ("static/" <> path)
    

