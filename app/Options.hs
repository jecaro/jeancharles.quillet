module Options (Options (..), getOptions) where

import Hakyll
  ( Configuration,
    defaultParserPrefs,
  )
import qualified Hakyll as H (Options, optionParser)
import qualified Options.Applicative as OA

data Options = Options
  { optCacheDirectory :: FilePath,
    optHakyllOptions :: H.Options
  }

getOptions :: String -> Configuration -> IO Options
getOptions progName conf =
  OA.customExecParser defaultParserPrefs (parserInfo progName conf)

optionParser :: Configuration -> OA.Parser Options
optionParser conf =
  Options
    <$> OA.strOption
      ( OA.long "cache-dir"
          <> OA.short 'c'
          <> OA.value "_cache-http"
          <> OA.showDefault
          <> OA.help "Cache directory for http requests"
      )
    <*> H.optionParser conf

parserInfo :: String -> Configuration -> OA.ParserInfo Options
parserInfo progName conf =
  OA.info
    (OA.helper <*> optionParser conf)
    ( OA.fullDesc
        <> OA.progDesc
          ( progName ++ " - Static site compiler created with Hakyll"
          )
    )
