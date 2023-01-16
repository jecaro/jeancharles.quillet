module PandocCompiler (pandocCompilerWithCode) where

import Data.List (lookup)
import qualified Data.Text as Text (length, lines, unlines, unpack)
import Hakyll
  ( Compiler,
    Item (..),
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    pandocCompilerWithTransformM,
  )
import Hakyll.Core.Compiler (unsafeCompiler)
import Network.HTTP.Req
  ( BsResponse,
    GET (..),
    NoReqBody (..),
    Option,
    Req,
    Scheme (..),
    Url,
    bsResponse,
    defaultHttpConfig,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Text.Pandoc (Block (..))
import Text.Pandoc.Walk (walkM)
import Text.URI (mkURI)
import Prelude hiding (Option)

pandocCompilerWithCode :: Compiler (Item String)
pandocCompilerWithCode =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (walkM codeFilter)

type ReqURL = Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)

data DistantText = DistantText
  { dtUrl :: ReqURL,
    dtFrom :: Maybe Int,
    dtTo :: Maybe Int,
    dtLanguage :: Maybe Text
  }

reqURL :: ReqURL -> Req BsResponse
reqURL = either (uncurry req') (uncurry req')
  where
    req' url = req GET url NoReqBody bsResponse

parse :: [(Text, Text)] -> Maybe DistantText
parse args = do
  url <- useURI =<< mkURI =<< lookup "url" args
  let from = lookup "from" args >>= readMaybe . Text.unpack
      to = lookup "to" args >>= readMaybe . Text.unpack
      language = lookup "language" args
  pure $
    DistantText
      { dtUrl = url,
        dtFrom = from,
        dtTo = to,
        dtLanguage = language
      }

fromTo :: Maybe Int -> Maybe Int -> Text -> Text
fromTo mbFrom mbTo text = Text.unlines . drop from . take to . Text.lines $ text
  where
    from = fromMaybe 1 mbFrom - 1
    to = fromMaybe (Text.length text) mbTo

codeFilter :: Block -> Compiler Block
codeFilter (CodeBlock (ident, ["get"], namevals) _) = do
  case parse namevals of
    Nothing -> fail "Invalid arguments"
    Just dt -> do
      let url = dtUrl dt
          from = dtFrom dt
          to = dtTo dt
          language = dtLanguage dt
      unsafeCompiler $ do
        response <- runReq defaultHttpConfig $ reqURL url
        let content = fromTo from to . decodeUtf8 $ responseBody response
        pure
          . CodeBlock (ident, ["sourceCode"] <> maybeToList language, namevals)
          $ content
codeFilter x = pure x
