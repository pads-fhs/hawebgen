module StatiGen.Config where

import System.FilePath
import System.Directory
import System.Exit
import System.IO
import Control.Monad
import Data.String.Utils
import Data.ConfigFile

import StatiGen.Util
import StatiGen.Types

-- Läd die allgemeine Konfigurationsdatei config.conf uns speichert deren Inhalt in den Datentyp
-- site und gibt diesen zurück.
getSiteConfig :: FilePath -> IO Site
getSiteConfig conf = do
  existsConfig <- doesFileExist conf
  when (existsConfig == False) $ do
    hPutStrLn stderr $ "Die Seite konnte nicht gebaut werden, da keine Config.conf Datei vorhanden ist!"
    exitWith $ ExitFailure 3
  contents <- readFile conf
  let site = do
        c <- readstring emptyCP contents
        author <- get c "DEFAULT" "author"
        date <- get c "DEFAULT" "year"
        title <- get c "DEFAULT" "website_title"
        slogan <- get c "DEFAULT" "website_slogan"
        description <- get c "DEFAULT" "description"
        keywords <- get c "DEFAULT" "keywords"
        return Site { siteAuthor = author
                      , siteDate = date
                      , siteTitle = title
                      , siteSlogan = slogan
                      , siteDescription = description
                      , siteKeywords = keywords
                      }
  case site of
    Left cperr -> error $ show cperr
    Right site -> return site

-- Läd den Inhalt einer Seite und deren Konfigurationsdatei in den Datentyp Page
-- und gibt diesen zurück.
getPageConfig :: FilePath -> IO Page
getPageConfig conf = do
  content <- getFirstElement $ split "." conf
  contents <- readFile ("src" </> conf)
  let page = do
        c <- readstring emptyCP contents
        title <- get c "DEFAULT" "title"
        inMenu <- get c "DEFAULT" "in_menu"
        order <- get c "DEFAULT" "order"
        let nav = NavEntry { navOrder = order
                             , navTitle = title 
                             , navUrl = content++".html"
                             }
        return Page {  pageTitle = title
                      , pageInMenu = inMenu
                      , pageNav = nav
                      , pageContent = content++".page"
                      }
  case page of
    Left cperr -> error $ show cperr
    Right page -> return page