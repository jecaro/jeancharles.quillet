module Cache (getContent) where

import Control.Exception (tryJust)
import Data.ByteString (unpack)
import Data.ByteString.Internal (w2c)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as Text (length, lines, unlines)
import Data.Text.IO as T (readFile, writeFile)
import DistantText (DistantText (..), ReqURL)
import Network.HTTP.Req
  ( BsResponse,
    GET (..),
    NoReqBody (..),
    Req,
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
  )
import System.Directory (createDirectoryIfMissing)
import System.IO.Error (isDoesNotExistError)

reqURL :: ReqURL -> Req BsResponse
reqURL = either (uncurry req') (uncurry req')
  where
    req' url = req GET url NoReqBody bsResponse

fromTo :: Maybe Int -> Maybe Int -> Text -> Text
fromTo mbFrom mbTo text = Text.unlines . drop from . take to . Text.lines $ text
  where
    from = fromMaybe 1 mbFrom - 1
    to = fromMaybe (Text.length text) mbTo

getContent :: FilePath -> DistantText -> IO Text
getContent cache (DistantText {..}) = do
  content <- whenNothingM (retrieveFrom cache dtHash) $ do
    content <- fetch dtUrl
    storeIn cache dtHash content
    pure content
  pure $ fromTo dtFrom dtTo content

fetch :: ReqURL -> IO Text
fetch url = decodeUtf8 . responseBody <$> runReq defaultHttpConfig (reqURL url)

storeIn :: FilePath -> ByteString -> Text -> IO ()
storeIn cache key txt = do
  createDirectoryIfMissing True cache
  T.writeFile (filename cache key) txt

filename :: FilePath -> ByteString -> FilePath
filename cache key = cache <> "/" <> (w2c <$> unpack key)

retrieveFrom :: FilePath -> ByteString -> IO (Maybe Text)
retrieveFrom cache key = eitherToMaybe <$> tryJust doesNotExist (T.readFile path)
  where
    doesNotExist e
      | isDoesNotExistError e = Just e
      | otherwise = Nothing
    path = filename cache key
