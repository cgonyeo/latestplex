{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Control.Monad
import System.Exit

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
    page <- getDownloadsPage
    let (success,msg) = parseCentosUrl page
    when (not success) $
        die (C.unpack msg)
    putStrLn (C.unpack msg)

url :: String
url = "https://plex.tv/downloads"

getDownloadsPage :: IO BS.ByteString
getDownloadsPage = simpleHttp url

-- Given an html page, returns the value of the href attribute on the tag
-- that has the attribute "data-event-label" set to "CentOS64". If multiple
-- tags are found where that is the case, returns an error.
parseCentosUrl :: BS.ByteString -> (Bool,BS.ByteString)
parseCentosUrl page =
    let tags = parseTags page
        centostags = filter (\x -> isTagOpen x &&
                        (fromAttrib "data-event-label" x == "CentOS64")) tags
    in case centostags of
           [tag] -> (True,fromAttrib "href" tag)
           otherwise -> (False,("Unexpected number of centos tags found: "
                              `BS.append` (C.pack $ show (length centostags))))
