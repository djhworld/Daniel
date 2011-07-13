module Daniel.Utils.Web (
    isShortUrl,
    getHtmlBody,
    getWebpageInfo,
    WebsiteInfo
)
where

import Network.HTTP (simpleHTTP, getRequest, getHeaders)
import Data.Maybe (fromJust)
import Network.Curl.Download
import Text.HTML.TagSoup

data WebsiteInfo = WebsiteInfo { url :: String, title :: String } deriving (Show)

isShortUrl :: String -> IO (Maybe String)
isShortUrl url = do
                req <- simpleHTTP (getRequest url)
                case req of
                    (Right x) -> do
                                    headers <- return $ getHeaders x
                                    mappy <- return $ map (\x -> (show (hdrName x), hdrValue x)) headers
                                    return (lookup "Location" mappy)
                    _ -> return Nothing

getHtmlBody :: String -> IO (Maybe String)
getHtmlBody url = do
                    contents <- openURIString url
                    case contents of
                        (Right x) -> return $ Just x
                        (Left y) -> return Nothing


getTitle :: String -> String
getTitle tags = parseForTitle $ parseTags tags


getWebpageInfo :: String -> IO (WebsiteInfo)
getWebpageInfo url = do
                        body <- getHtmlBody url
                        title <- return $  getTitle $ fromJust body
                        return $ WebsiteInfo url title

parseForTitle :: [Tag String] -> String
parseForTitle xs = innerText $ takeWhile (~/= TagClose "title") $ dropWhile (~/= TagOpen "title" []) xs
