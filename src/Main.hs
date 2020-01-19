{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Main
  ( main
  , ghcidEntry
  ) where

import Prelude

import Control.Monad (void, filterM)
import Control.Monad.Error.Class (throwError)
import Data.Functor ((<&>))
import Data.List (intercalate)
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
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "img/*" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $
    compile templateBodyCompiler

  match "writing/*" $ do
    route $ customRoute dateRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/article.html" defaultContext
      >>= loadAndApplyTemplate "templates/root.html" defaultContext
      >>= relativizeUrls

  match "talks/*" $ do
    route $ customRoute dateRoute
    compile $ revealJsCompiler >>= relativizeUrls

  create ["writing.html"] $ do
    route idRoute
    compile $ do
      articles <- loadAll "writing/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/writing.html" (postCtx articles)
        >>= loadAndApplyTemplate "templates/root.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
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
    applyRevealJs (Item ident body) = cached "Main.applyRevealJs" $
      case runPure (Item ident . unpack <$> writeRevealJs writerOptions body) of
        Left err -> throwError ["Main.applyRevealJs: " <> show err]
        Right item -> pure item

postCtx :: [Item String] -> Context String
postCtx articles =
     constField "title" "Writing"
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
