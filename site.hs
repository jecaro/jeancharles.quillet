{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll
  ( Compiler,
    Context,
    FeedConfiguration (..),
    Identifier,
    Item (..),
    Pattern,
    applyAsTemplate,
    bodyField,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultContext,
    field,
    fromGlob,
    getItemModificationTime,
    getRoute,
    hakyll,
    idRoute,
    listField,
    loadAll,
    loadAllSnapshots,
    loadAndApplyTemplate,
    makeItem,
    match,
    matches,
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

main :: IO ()
main = hakyll $ do
  -- Copy the assets
  forM_ ["robots.txt", "assets/*.js"] $ \p -> match p $ do
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
      let archiveCtx =
            constField "title" "Posts"
              <> listField postsString postCtx (pure posts)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= loadAndApplyTemplate defaultTemplate archiveCtx
        >>= relativizeUrls

  -- One page per post
  match postsPattern $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
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
  match "pages/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate defaultTemplate defaultContext
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
            listField "pages" (lastmodCtx <> priorityCtx <> urlCtx) (pure pages)
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

-- The contexts

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

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
        pure $ root <> (fromMaybe mempty r)
    root = "https://jeancharles.quillet.org/"

lastmodCtx :: Context String
lastmodCtx = field "lastmod" $ lastmod <=< mostRecent
  where
    -- Return the last modification time of an item
    lastmod = pure . formatTimeDefault <=< getItemModificationTime . itemIdentifier
    -- Get the last post item when called from the posts page or the item
    -- itself in any other case
    mostRecent item@(Item identifier _)
      | identifier == postsIdentifier = pure . headOrItem =<< recentPosts
      | otherwise = pure item
      where
        headOrItem = fromMaybe item . viaNonEmpty head

    formatTimeDefault = formatTime defaultTimeLocale "%F"

-- Strings and patterns

postsPattern :: Pattern
postsPattern = "posts/*"

postsIdentifier :: Identifier
postsIdentifier = "pages/posts.html"

postsString :: String
postsString = "posts"

pagesPattern :: Pattern
pagesPattern = "pages/*"

indexPattern :: Pattern
indexPattern = fromGlob indexString

indexString :: String
indexString = "index.md"

defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"

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
