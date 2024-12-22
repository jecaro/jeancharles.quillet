{-# LANGUAGE TemplateHaskell #-}

module Compilers (rules) where

import Data.List.Extra (nubOrdOn)
import Data.Time (defaultTimeLocale, toGregorian, utctDay)
import Data.Tuple.Extra (fst3)
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
    boolField,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultContext,
    field,
    fromGlob,
    getItemUTC,
    getMetadataField,
    getRoute,
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
import PandocCompiler (pandocCompilerWithCode)

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
      compile postsCompiler

    -- One page per post
    match postPattern $ do
      route $ setExtension "html"
      compile $ postCompiler cache

    -- The rss feed of the posts
    create ["rss.xml"] $ do
      route idRoute
      compile rssCompiler

    -- Other pages
    match pagePattern $ do
      route $ setExtension "html"
      compile pageCompiler

    -- And the index
    match indexPattern $ do
      route $ setExtension "html"
      compile indexCompiler

    -- The templates
    match "templates/*" $ compile templateBodyCompiler

    -- The site map
    create ["sitemap.xml"] $ do
      route idRoute
      compile siteMapCompiler

-- The compilers

postsCompiler :: Compiler (Item String)
postsCompiler = do
  posts <- getRecentPosts

  let identifiers = itemIdentifier <$> posts
  years <- traverse getYear identifiers

  let yearsAndIdentifiers = zip years identifiers
      -- list of posts that are the first/last published in their year
      -- used for the loop in the template
      firstInYear = snd <$> nubOrdOn fst yearsAndIdentifiers
      lastInYear = snd <$> nubOrdOn fst (reverse yearsAndIdentifiers)

      recentPostsCtx =
        inListField "first" firstInYear
          <> inListField "last" lastInYear
          <> dateField "date" "%B %e"
          <> dateField "year" "%Y"
          <> defaultContext

      archivesCtx =
        constField "title" "Posts"
          <> listField postsString recentPostsCtx (pure posts)
          <> defaultContext

  makeItem ""
    >>= loadAndApplyTemplate "templates/posts.html" archivesCtx
    >>= loadAndApplyTemplate defaultTemplate archivesCtx
    >>= relativizeUrls
  where
    getYear = fmap (fst3 . toGregorian . utctDay) . getItemUTC defaultTimeLocale

postCompiler :: FilePath -> Compiler (Item String)
postCompiler cache = do
  pandocCompilerWithCode cache
    -- We save the body to the field content to use it in the rss feed
    -- later on
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate defaultTemplate postCtx
    >>= relativizeUrls

rssCompiler :: Compiler (Item String)
rssCompiler = do
  posts <-
    fmap (take 10) . recentFirst
      =<< loadAllSnapshots postPattern "content"
  renderRss feedConfiguration feedCtx posts
  where
    feedCtx = postCtx <> bodyField "description"
    feedConfiguration =
      FeedConfiguration
        { feedTitle = "All posts",
          feedDescription = "Jean-Charles personal blog",
          feedAuthorName = "Jean-Charles Quillet",
          feedAuthorEmail = "jeancharles.quillet@gmail.com",
          feedRoot = "https://jeancharles.quillet.org"
        }

pageCompiler :: Compiler (Item String)
pageCompiler =
  pandocCompiler
    >>= loadAndApplyTemplate defaultTemplate (defaultContext <> gitCtx)
    >>= relativizeUrls

indexCompiler :: Compiler (Item String)
indexCompiler = do
  posts <- take 10 <$> getRecentPosts
  let indexCtx =
        listField postsString postCtx (pure posts)
          <> defaultContext

  pandocCompiler
    -- to have the partial render as html
    >>= applyAsTemplate indexCtx
    >>= loadAndApplyTemplate defaultTemplate indexCtx
    >>= relativizeUrls

siteMapCompiler :: Compiler (Item String)
siteMapCompiler = do
  pages <- loadAll $ indexPattern .||. pagePattern .||. postPattern
  let sitemapCtx =
        listField "pages" (lastModCtx <> priorityCtx <> urlCtx) (pure pages)
  makeItem ("" :: String)
    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

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
      | matches postsPattern identifier = do
          postsIdentifiers <- fmap itemIdentifier <$> getRecentPosts
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

inListField :: String -> [Identifier] -> Context a
inListField name = boolField name . identifierIn
  where
    identifierIn = flip (elem . itemIdentifier)

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

postPattern :: Pattern
postPattern = "posts/*"

postsPattern :: Pattern
postsPattern = fromGlob "pages/posts.html"

postsIdentifier :: Identifier
postsIdentifier = "pages/posts.html"

postsString :: String
postsString = "posts"

pagePattern :: Pattern
pagePattern = "pages/**"

indexPattern :: Pattern
indexPattern = fromGlob indexString

indexString :: String
indexString = "index.md"

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

lastModString :: String
lastModString = "lastmod"

getRecentPosts :: Compiler [Item String]
getRecentPosts = recentFirst =<< loadAll postPattern

