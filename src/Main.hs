{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main
  ( main
  , ghcidEntry
  ) where

import Prelude

import Control.Monad (void, filterM)
import Control.Monad.Error.Class (throwError)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (intercalate, sortBy)
import Data.List.Extra (split)
import Data.Text (unpack)
import Hakyll
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Writers (writeRevealJs)

main :: IO ()
main =
  hakyllWithArgs config (Options False Build) buildRules

ghcidEntry :: IO ()
ghcidEntry = do
  void $ hakyllWithExitCodeAndArgs config (Options False Clean) buildRules
  void $ hakyllWithExitCodeAndArgs config (Options False Build) buildRules
  void $ hakyllWithExitCodeAndArgs config (Options False (Watch "127.0.0.1" 8000 False)) buildRules

config :: Configuration
config = defaultConfiguration

buildRules :: Rules ()
buildRules = do
  match ("css/*" .||. "css/**/*") do
    route idRoute
    compile compressCssCompiler

  match ("assets/*" .||. "assets/**/*") do
    route idRoute
    compile copyFileCompiler

  match ("js/*" .||. "js/**/*") do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $
    compile templateBodyCompiler

  match "writing/*" do
    route $ customRoute dateRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/article.html" defaultContext
      >>= loadAndApplyTemplate "templates/navbar.html" defaultContext
      >>= loadAndApplyTemplate "templates/root.html" defaultContext
      >>= relativizeUrls

  match "talks/*" do
    route (customRoute dateRoute)
    compile (revealJsCompiler >>= relativizeUrls)

  create ["talks.html"] do
    route idRoute
    compile do
      articles <- loadAll "talks/*" <&> sortBy (compare `on` itemIdentifier) <&> reverse
      makeItem ""
        >>= loadAndApplyTemplate "templates/talks.html" (postCtx "Talks" articles)
        >>= loadAndApplyTemplate "templates/navbar.html" defaultContext
        >>= loadAndApplyTemplate "templates/root.html" defaultContext
        >>= relativizeUrls

  create ["writing.html"] do
    route idRoute
    compile do
      articles <- loadAll "writing/*" <&> sortBy (compare `on` itemIdentifier) <&> reverse
      makeItem ""
        >>= loadAndApplyTemplate "templates/writing.html" (postCtx "Writing" articles)
        >>= loadAndApplyTemplate "templates/navbar.html" defaultContext
        >>= loadAndApplyTemplate "templates/root.html" defaultContext
        >>= relativizeUrls

  match "index.html" do
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/root.html" defaultContext
      >>= relativizeUrls

revealJsCompiler :: Compiler (Item String)
revealJsCompiler
  =   getResourceBody
  >>= readPandoc
  >>= applyRevealJs
  >>= loadAndApplyTemplate "templates/slides.html" defaultContext
  where
    writerOptions =
      defaultHakyllWriterOptions { writerHighlightStyle = Nothing }

    applyRevealJs :: Item Pandoc -> Compiler (Item String)
    applyRevealJs (Item ident body) =
      let
        newItem = Item ident . unpack <$> writeRevealJs writerOptions body
      in
        cached "Main.applyRevealJs" case runPure newItem of
          Left err -> throwError ["Main.applyRevealJs: " <> show err]
          Right item -> pure item

postCtx :: String -> [Item String] -> Context String
postCtx title articles =
     constField "title" title
  <> listField "articles" defaultContext (filterM published articles)
  <> defaultContext
  where
    published item =
      getMetadataField (itemIdentifier item) "published" <&> \case
        Just "true" -> True
        Just _ -> False
        Nothing -> False

dateRoute :: Identifier -> FilePath
dateRoute (toFilePath -> original) =
  parseDate . split (== '-') $ original
  where
    parseDate = \case
      (year : month : date : rest) ->
        year <> "/" <> month <> "/" <> date <> "/" <> withExtension "html" rest
      _unsupported -> original
    withExtension ext =
      (<> ext) . reverse . dropWhile (/= '.') . reverse . intercalate "-"
