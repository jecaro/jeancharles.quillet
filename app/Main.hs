{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.GitRev (gitHash)
import Hakyll
  ( Compiler,
    Context,
    FeedConfiguration (..),
    Identifier,
    Item (..),
    Pattern,
    Rules,
    applyAsTemplate,
    bodyField,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultConfiguration,
    defaultContext,
    field,
    fromGlob,
    getMetadataField,
    getRoute,
    hakyllWithExitCodeAndArgs,
    idRoute,
    listField,
    loadAll,
    loadAllSnapshots,
    loadAndApplyTemplate,
    makeItem,
    match,
    matches,
    noResult,
    pandocCompiler,
    recentFirst,
    relativizeUrls,
    renderRss,
    route,
    saveSnapshot,
    setExtension,
    templateBodyCompiler,
    (.||.),
  )
import Options (Options (..), getOptions)
import PandocCompiler (pandocCompilerWithCode)
import System.Environment (getProgName)

rules :: FilePath -> Rules ()
rules cache =
  do
    -- Copy the assets
    forM_ ["robots.txt", "assets/*.js", "images/*"] $ \p -> match p $ do
      route idRoute
      compile copyFileCompiler

    match "assets/*.css" $ do
      route idRoute
      compile compressCssCompiler

    -- Create the page with the posts list
    create [postsIdentifier] $ do
      route idRoute
      compile $ do
        posts <- recentPosts
        let archivesCtx =
              constField "title" "Posts"
                <> listField postsString postCtx (pure posts)
                <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" archivesCtx
          >>= loadAndApplyTemplate defaultTemplate archivesCtx
          >>= relativizeUrls

    -- One page per post
    match postsPattern $ do
      route $ setExtension "html"
      compile $
        pandocCompilerWithCode cache
          -- We save the body to the field content to use it in the rss feed
          -- later on
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate defaultTemplate postCtx
          >>= relativizeUrls

    -- The rss feed of the posts
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshots postsPattern "content"
        renderRss feedConfiguration feedCtx posts

    -- Other pages
    match pagesPattern $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate defaultTemplate (defaultContext <> gitCtx)
          >>= relativizeUrls

    -- And the index
    match indexPattern $ do
      route $ setExtension "html"
      compile $ do
        posts <- take 10 <$> recentPosts
        let indexCtx =
              listField postsString postCtx (pure posts)
                <> defaultContext

        pandocCompiler
          -- to have the partial render as html
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate defaultTemplate indexCtx
          >>= relativizeUrls

    -- The templates
    match "templates/*" $ compile templateBodyCompiler

    -- The site map
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        pages <- loadAll $ indexPattern .||. pagesPattern .||. postsPattern
        let sitemapCtx =
              listField "pages" (lastModCtx <> priorityCtx <> urlCtx) (pure pages)
        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

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

-- The contexts

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> constField "show-title" "true"
    <> defaultContext

lastModCtx :: Context String
lastModCtx = field lastModString lastMod
  where
    lastMod (Item identifier _)
      | matches archivesPattern identifier = do
          postsIdentifiers <- fmap itemIdentifier <$> recentPosts
          lastMods <- traverse getLastMod postsIdentifiers
          failWhenNothing identifier $ lastOfAllMods lastMods
      | otherwise = failWhenNothing identifier =<< getLastMod identifier
    failWhenNothing identifier =
      maybe
        ( noResult $
            "Unable to get last modification metadata from: "
              <> show identifier
        )
        pure
    getLastMod = flip getMetadataField lastModString
    lastOfAllMods = listToMaybe . sort . catMaybes

-- Make the git hash appear in the meta tag of the contact page
gitCtx :: Context String
gitCtx = field "version" version
  where
    version (Item "pages/contact.md" _) = pure $ take 7 $(gitHash)
    version _ = noResult ""

priorityCtx :: Context String
priorityCtx = field "priority" $ pure . show . priority
  where
    priority :: Item a -> Double
    priority (Item identifier _)
      | matches indexPattern identifier = 1
      | otherwise = 0.8

urlCtx :: Context String
urlCtx = field "url" url
  where
    url (Item identifier _)
      | matches indexPattern identifier = pure root
      | otherwise = do
          r <- getRoute identifier
          pure $ root <> fromMaybe mempty r
    root = "https://jeancharles.quillet.org/"

-- Strings and patterns

postsPattern :: Pattern
postsPattern = "posts/*"

archivesPattern :: Pattern
archivesPattern = fromGlob "pages/posts.html"

postsIdentifier :: Identifier
postsIdentifier = "pages/posts.html"

postsString :: String
postsString = "posts"

pagesPattern :: Pattern
pagesPattern = "pages/**"

indexPattern :: Pattern
indexPattern = fromGlob indexString

indexString :: String
indexString = "index.md"

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

lastModString :: String
lastModString = "lastmod"

-- Others

recentPosts :: Compiler [Item String]
recentPosts = recentFirst =<< loadAll postsPattern

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "All posts",
      feedDescription = "Jean-Charles personal blog",
      feedAuthorName = "Jean-Charles Quillet",
      feedAuthorEmail = "jeancharles.quillet@gmail.com",
      feedRoot = "https://jeancharles.quillet.org"
    }
