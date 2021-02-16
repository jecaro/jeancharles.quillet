{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do
  match "assets/*.js" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "*.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler
