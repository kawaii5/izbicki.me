--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import Data.Monoid 
import Debug.Trace
import System.Directory
import System.FilePath

import Hakyll
import Hakyll.Core.Routes
import Text.Pandoc
import Text.Regex

--------------------------------------------------------------------------------

main :: IO ()
main =  hakyll $ do

    -- global configuration
    let pandocOptions = defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax "" 
            }

    -- process static files
    directory static "img"

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- process blog post
    match "blog/*.md" $ do
        route $ constRoute "index.html"

        compile $ do
            path <- toFilePath <$> getUnderlying

            pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
                >>= loadAndApplyTemplate "templates/post.html"  postCtx
                >>= loadAndApplyTemplate "templates/draft.html" postCtx
                >>= relativizeUrls

    -- process templates
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

static :: Pattern -> Rules ()
static f = match f $ do
    route   idRoute
    compile copyFileCompiler

directory :: (Pattern -> Rules a) -> String -> Rules a
directory act f = act $ fromGlob $ f ++ "/**"

