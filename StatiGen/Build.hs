module StatiGen.Build where

import Paths_StatiGen
import System.FilePath
import System.Directory
import System.Exit
import System.IO
import Control.Monad
import Text.Pandoc

import StatiGen.Config
import StatiGen.Util
import StatiGen.Types

-- Es werden alle Daten eingelesen und daraus die fertige Webseite gebaut.
buildSite :: IO ()
buildSite = do
  existsWebsite <- checkWebsite
  when (existsWebsite == False) $ do
    hPutStrLn stderr $ "Es konnte keine Website gefunden werden!"
    exitWith $ ExitFailure 1
  config <- getSiteConfig "config.conf"
  template <- getTemplate
  content <- getDirectoryContents "src"
  let content' = filter (\f -> let ext = takeExtension f in ext == ".conf") $ filter (`notElem` ["..","."]) content
  pages <- mapM getPageConfig content'
  navigation <- getNavigation pages
  forM_ pages $ \page -> do
    renderHtmlPage config template navigation page
  existsCss <- doesFileExist ("src" </> "default.css")
  when existsCss $ do
    copyFile ("src" </> "default.css") ("output" </> "default.css")
  existsImages <- doesDirectoryExist ("src" </> "images")
  when existsCss $ do
    templateContent <- liftM (filter (/=".") . map (makeRelative ("src" </> "images"))) $ getDirectoryContentsRecursive ("src" </> "images")
    forM_ templateContent $ \file -> do
    let dest = "output" </> "images" </> file
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile ("src" </> "images" </> file) dest

-- Ersetzt die Platzhalter im Template mit den richtigen Daten und erzeugt daraus eine HTML-Datei.
renderHtmlPage :: Site -> [Char] -> [Char] -> Page -> IO ()
renderHtmlPage site template navigation page = do
  content <- readFile ("src" </> (pageContent page))
  let contentHtml = markdownToHTML content
  let siteWithContent = replace template "{content:}" contentHtml
  let siteWithPageTitle = replace siteWithContent "{title:}" (pageTitle page)
  let siteWithSiteDescription = replace siteWithPageTitle "{description:}" (siteDescription site)
  let siteWithSitesiteKeywords = replace siteWithSiteDescription "{keywords:}" (siteKeywords site)
  let siteWithSiteTitle = replace siteWithSitesiteKeywords "{website_title:}" (siteTitle site)
  let siteWithSlogan = replace siteWithSiteTitle "{website_slogan:}" (siteSlogan site)
  let siteWithDate = replace siteWithSlogan "{year:}" (siteDate site)
  let siteWithAuthor = replace siteWithDate "{author:}" (siteAuthor site)
  let completeSite = replace siteWithAuthor "{menu:}" $ setCurrentSiteInNavigation navigation page
  let path = navUrl $ pageNav page
  writeFile ("output" </> path) completeSite

-- Wandelt einen String von Markdown in HTML.
markdownToHTML :: String -> String
markdownToHTML string = writeHtmlString defaultWriterOptions (readMarkdown defaultParserState string)
  
-- Erzeugt die Navigation.
getNavigation :: Monad m => [Page] -> m [Char]
getNavigation pages = do
  navEntries <- mapM getNavEntries $ filter (\page -> pageInMenu page == True) pages
  let navEntriesWithOrder = quickSort navEntries
  navi <- mapM buildNavigation navEntriesWithOrder
  return (concat navi)

-- Gibt ein NavEntry einer Page zurück.
getNavEntries :: Monad m => Page -> m NavEntry
getNavEntries page = do
  let nav = pageNav page
  return nav

-- Baut den HTML Navigationseintrag für ein NavEntry
buildNavigation :: Monad m => NavEntry -> m [Char]
buildNavigation entry = do
  let entryHtml = "<li><a href=\""++(navUrl entry)++"\">"++(navTitle entry)++"</a></li>"
  return entryHtml

-- Setzt den Navigationseintrag für die übergebene Seite auf selected.
setCurrentSiteInNavigation :: [Char] -> Page -> [Char]
setCurrentSiteInNavigation navigation page = do
  newNavigation <- replace ("<ul>"++navigation++"</ul>") ("<li><a href=\""++(navUrl $ pageNav page)++"\">") ("<li class=\"selected\"><a href=\""++(navUrl $ pageNav page)++"\">")
  return newNavigation

-- Ändert das Template auf das angegebene Template.
setTemplate :: FilePath -> IO ()
setTemplate template = do
  existsWebsite <- checkWebsite
  when (existsWebsite == False) $ do
    hPutStrLn stderr $ "Es konnte keine Website gefunden werden!"
    exitWith $ ExitFailure 1
  templateDir <- getDataFileName "templates"
  existsTemplate <- doesDirectoryExist (templateDir </> template)
  when (existsTemplate == False) $ do
    hPutStrLn stderr $ "Eine Template mit dem Namen " ++ template ++ " existiert nicht."
    exitWith $ ExitFailure 3
  templateContent <- liftM (filter (/=".") . map (makeRelative (templateDir </> template))) $ getDirectoryContentsRecursive (templateDir </> template)
  forM_ templateContent $ \file -> do
    let dest = "src" </> file
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile ((templateDir </> template) </> file) dest

-- Fügt eine neue Seite hinzu (.page und .conf Datei).
addPage :: [Char] -> IO ()
addPage name = do
  existsWebsite <- checkWebsite
  when (existsWebsite == False) $ do
    hPutStrLn stderr $ "Es konnte keine Website gefunden werden!"
    exitWith $ ExitFailure 1
  writeFile ("src"</>name++".conf") "title: Neue Seite\nin_menu: true\norder: 1"
  writeFile ("src"</>name++".page") "## Beispielseite\n\nDies ist nur eine Beispielseite."