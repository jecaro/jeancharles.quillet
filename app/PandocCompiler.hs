module PandocCompiler (pandocCompilerWithCode) where

import Cache (getContent)
import DistantText (DistantText (..), parse)
import Hakyll
  ( Compiler,
    Item (..),
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    pandocCompilerWithTransformM,
  )
import Hakyll.Core.Compiler (unsafeCompiler)
import Text.Pandoc (Block (..))
import Text.Pandoc.Walk (walkM)

pandocCompilerWithCode :: FilePath -> Compiler (Item String)
pandocCompilerWithCode cache =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (walkM (codeFilter cache))

codeFilter :: FilePath -> Block -> Compiler Block
codeFilter cache (CodeBlock (ident, ["get"], namevals) _) = do
  case parse namevals of
    Nothing -> fail "Invalid arguments"
    Just dt@DistantText {..} -> do
      unsafeCompiler $ do
        content <- getContent cache dt
        pure
          . CodeBlock (ident, ["sourceCode"] <> maybeToList dtLanguage, namevals)
          $ content
codeFilter _ x = pure x
