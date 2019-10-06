{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Main
  where
--  ( main
--  , ghcidEntry
--  ) where

import Prelude

import Data.List (intercalate)
import Data.List.Extra (split)
import Data.Functor ((<&>))
import Control.Monad (void, filterM)
import Hakyll


import Control.Monad.Reader

data App = App
  { logLn :: forall a. Show a => a -> IO ()
  }

class Persist m where
  put :: Int -> m ()

data Persister m = Persister { _put :: Int -> m () }

instance MonadReader (Persister m) m => Persist m where
  put i = do
    persister <- ask
    (_put persister) i

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

  match "js/*" $ do
    route $ gsubRoute "docs/" (const "")
    compile copyFileCompiler

  match "templates/*" $
    compile templateBodyCompiler

  match "writing/*" $ do
    route $ customRoute dateRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/article.html" defaultContext
      >>= loadAndApplyTemplate "templates/root.html" defaultContext
      >>= relativizeUrls

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
