module DistantText (DistantText (..), ReqURL, parse) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 (encode)
import Data.List (lookup)
import qualified Data.Text as Text (unpack)
import Network.HTTP.Req (Option, Scheme (..), Url, useURI)
import Text.URI (mkURI, renderBs)

type ReqURL = Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)

data DistantText = DistantText
  { dtUrl :: !ReqURL,
    dtHash :: !ByteString,
    dtFrom :: !(Maybe Int),
    dtTo :: !(Maybe Int),
    dtLanguage :: !(Maybe Text)
  }

parse :: [(Text, Text)] -> Maybe DistantText
parse args = do
  uri <- mkURI =<< lookup "url" args
  url <- useURI uri
  let from = lookup "from" args >>= readMaybe . Text.unpack
      to = lookup "to" args >>= readMaybe . Text.unpack
      language = lookup "language" args
  pure $
    DistantText
      { dtUrl = url,
        dtHash = encode . hash $ renderBs uri,
        dtFrom = from,
        dtTo = to,
        dtLanguage = language
      }
