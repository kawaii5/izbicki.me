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
-- import qualified Text.Regex.Base.RegexLike as Regex
-- import qualified Text.Regex.TDFA as Regex
import Text.Regex

--------------------------------------------------------------------------------
main :: IO ()
main = do
  cats <- findCategories
  hakyll $ do

    -- global configuration
    let pandocOptions = defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax "" 
            }

    -- process static files
    directory static "img"

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "htaccess" $ do
        route   $ constRoute ".htaccess"
        compile copyFileCompiler

    -- rss feed
    let myFeedConfiguration :: FeedConfiguration
        myFeedConfiguration = FeedConfiguration
            { feedTitle       = "Mike Izbicki's blog posts"
            , feedDescription = "My thoughts on computer science, religion, and other fun things"
            , feedAuthorName  = "Mike Izbicki"
            , feedAuthorEmail = "mike@izbicki.me"
            , feedRoot        = "http://izbicki.me/blog"
            }

    create ["feed.rss"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx 
                       <> bodyField "description"
--                        <> constField "description" "This is the post description"

--             posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**" -- "content"
--             posts <- {-fmap (take 10) .-} recentFirst =<< loadAllSnapshots ("blog/**" .&&. hasNoVersion) "content"
            posts <- {-fmap (take 10) .-} recentFirst =<< loadAllSnapshots ("blog/**" .&&. hasVersion "source") "content"
            renderAtom myFeedConfiguration feedCtx posts

    -- process blog posts
    match "blog/**" $ version "source" $ do
        route $ setExtension "html"
--         route $ rmExtension
              `composeRoutes`
              rmDate
              `composeRoutes`
              rmCategory
        compile $ do
            pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
--                 >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= amazonURLs
                >>= saveSnapshot "content"
                >>= relativizeUrls

    match "blog/**" $ do
        route $ setExtension "html"
--         route $ rmExtension
              `composeRoutes`
              rmDate
              `composeRoutes`
              rmCategory

        compile $ do
            path <- toFilePath <$> getUnderlying
--             let catCtx = constField "catlist" path

            catlist <- concat <$> mapM (dispCategory' path True) cats
            let catCtx = constField "catlist" catlist
                      <> constField "relatedposts" "True"
            
            pandocCompilerWith defaultHakyllReaderOptions pandocOptions 
--             >>= applyAsTemplate catCtx
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= amazonURLs
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" (postCtx <> catCtx)
                >>= relativizeUrls

--     create ["archive.html"] $ do
--         route idRoute
--         compile $ do
--             posts <- recentFirst =<< loadAll "posts/*"
--             let archiveCtx =
--                     listField "posts" postCtx (return posts) `mappend`
--                     constField "title" "Archives"            `mappend`
--                     defaultContext
-- 
--             makeItem ""
--                 >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                 >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                 >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
--             recentPosts <- do
--                 forM cats $ \cat -> do
--                     q <- loadAll (catpat cat)
--                     recentFirst q

--             let posts = foldl1 (<>) $ flip map cats $ \cat -> 

--             let postList cat = listField "posts" postCtx (recentFirst =<< loadAll (catpat cat))
--                 posts = listField "cats" (return []) (return cats)
            
            let recentPosts = fmap (take 5) $ recentFirst =<< loadAll ("blog/**" .&&. hasNoVersion)

            catlist <- concat <$> mapM dispCategory cats

            let indexCtx = constField "catlist" catlist -- $ dispCategories catlist
                        <> constField "noheader" "True"
                        <> listField "recentPosts" postCtx recentPosts
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= amazonURLs
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

rmDate :: Routes
rmDate = gsubRoute "[1234567890][1234567890][1234567890][1234567890]-[1234567890][1234567890]-[1234567890][1234567890]-" (const "")

rmCategory :: Routes
-- rmCategory = gsubRoute "([:alphanum:]*)/([:alphanum:]|[\\-])*/([:alphanum:]|[\\-])*" (const "\\1/\\2")
-- rmCategory = gsubRoute "posts/(**)" (const "\\1")
rmCategory = customRoute $ go . toFilePath
    where
        go str = if length xs > 1
            then head xs ++ "/" ++ last xs
            else head xs
            where xs = splitOn "/" str

rmExtension :: Routes
rmExtension = customRoute $ dropExtension . toFilePath

-------------------

amazonURLs :: Item String -> Compiler (Item String)
amazonURLs item = return $ fmap (addAmazonRef . simplifyAmazon) item 

simplifyAmazon :: String -> String
simplifyAmazon str = subRegex regex str replace
    where
        regex = mkRegex "http://(www.)*amazon.com/[A-Za-z0-9\\-]*/*(dp|gp/product)/([A-Za-z0-9]*)([/A-Za-z0-9=?_&\\-])*"
        replace = "http://www.amazon.com/dp/\\3"

addAmazonRef :: String -> String
addAmazonRef str = subRegex regex str replace
    where
        replace = "\\1?tag="++refid
        refid = "izbickime-20"
        regex = mkRegex "(http://amazon.com/[:alphanum:]|[-/]*dp/[1234567890]*)" 

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

-------------------

data CatInfo = CatInfo
    { catpat  :: Pattern
    , catpath :: String
    , desc    :: String
    , catname :: String
    , level   :: Int
    , subcats :: [CatInfo]
    }
    deriving Show

findCategories :: IO [CatInfo]
findCategories = do
    xs <- getSubDirs "blog"
    mapM go xs
    where
        go :: FilePath -> IO CatInfo -- [FilePath]
        go path = do    
            xs <- getSubDirs path
            xs' <- mapM go xs

            let descfile = path++"/.desc"
            dfe <- doesFileExist descfile
            desc <- if dfe
                then readFile (path++"/.desc")
                else return ""

            return $ CatInfo
                { catpat  = fromGlob $ path++"/*"
                , catpath = path
                , desc    = desc
                , catname = space2dash $ last splits
                , level   = length splits
                , subcats = xs'
                }
            where
                splits = splitOn "/" path

        getSubDirs path = do
            xs <- getDirectoryContents path
            xs' <- filterM doesDirectoryExist $ map ((path++"/")++) xs
            return $ sort $ filter (\x -> last x /= '.') xs'

space2dash :: String -> String
space2dash = map (\x -> if x=='-' then ' ' else x)

dash2space :: String -> String
dash2space = map (\x -> if x==' ' then '-' else x)

dispCategory :: CatInfo -> Compiler (String)
dispCategory cat = do
--     posts <- recentFirst =<< loadAll (catpat cat .&&. hasVersion "source")
    posts <- recentFirst =<< loadAll (catpat cat .&&. hasNoVersion )

    dispsubcats <- mapM dispCategory $ subcats cat

    let archiveCtx 
          = listField "posts" postCtx (return posts) 
         <> constField "level" (show (level cat+2))
         <> constField "catname" (catname cat)
         <> constField "dispsubcats" (concat dispsubcats)
         <> constField "desc" (desc cat)
         <> constField "listposts" "True"
         <> constField "dispdesc" "True"
         <> constField "catinside" "True"
         <> defaultContext

    fmap itemBody $ makeItem ""
        >>= loadAndApplyTemplate "templates/cat.html" archiveCtx

dirname :: FilePath -> FilePath
dirname path = concat $ intersperse "/"  $ init $ splitOn "/" path

init2 :: [a] -> [a]
init2 (x:y:[]) = x:y:[]
init2 xs = init xs

dispCategory' :: FilePath -> Bool -> CatInfo -> Compiler (String)
dispCategory' path recurse cat = if not (isPrefixOf (concat $ intersperse "/" $ init2 $ splitOn "/" $ catpath cat) path)
                                 && not (isPrefixOf (dirname path) (catpath cat))
    then return "" -- $ "<br/>dirname path=" ++ dirname path ++ "; catpath cat="++(concat $ init $ splitOn "/" $ catpath cat)
    else do
        posts <- loadAll (catpat cat .&&. hasVersion "source") 
             >>= recentFirst 

        let posts1 = takeWhile (\x -> toFilePath (itemIdentifier x) /= path) posts
            posts2 = [posts !! length posts1]
            posts3 = drop (length posts1+1) posts

        dispsubcats <- if not recurse
            then return []
            else if isPrefixOf (dirname path) (catpath cat)
                then fmap concat $ mapM (dispCategory' path False) $ subcats cat
                else fmap concat $ mapM (dispCategory' path True) $ subcats cat

        let dispsubcats' = if dispsubcats==""
                then defaultContext
                else constField "dispsubcats" dispsubcats
                  <> constField "catinside" "True"

        let listposts = if dirname path == catpath cat
                then constField "listposts" "True"
                  <> constField "catinside" "True"
                else defaultContext 

        let clickhere = if dirname path == catpath cat
                then defaultContext 
                else constField "dispclickme" "True"

        let archiveCtx 
              = listField "posts" postCtx (return posts1) 
             <> listField "posts2" postCtx (return posts2)
             <> listField "posts3" postCtx (return posts3) 
             <> constField "level" (show (level cat+2))
             <> constField "catname" (catname cat)
             <> constField "desc" (desc cat)
             <> dispsubcats'
             <> clickhere
             <> listposts
--              <> constField "debug" ("<b>debug</b>: cat="++show cat++"; dirname path="++dirname path++"; catpath cat="++catpath cat)
             <> defaultContext

        fmap itemBody $ makeItem ""
            >>= loadAndApplyTemplate "templates/cat.html" archiveCtx


