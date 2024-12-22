module Main (main) where

import Compilers (rules)
import Hakyll
  ( defaultConfiguration,
    hakyllWithExitCodeAndArgs,
  )
import Options (Options (..), getOptions)
import System.Environment (getProgName)

main :: IO ()
main = do
  progName <- getProgName
  Options {..} <- getOptions progName conf
  exitWith
    =<< hakyllWithExitCodeAndArgs
      conf
      optHakyllOptions
      (rules optCacheDirectory)
  where
    conf = defaultConfiguration
