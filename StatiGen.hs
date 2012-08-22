module Main where

import Paths_StatiGen
import System.FilePath
import System.Environment
import System.Directory
import System.Exit
import System.IO
import Control.Monad

import StatiGen.Util
import StatiGen.Build

-- Startpunkt des Programms
-- Anhand der angegebenen Parameter wird die entsprechende Funktion aufgerufen.
main :: IO ()
main = do
  args <- getArgs
  case args of
       ["create",site]          -> create site "default" >> exitWith ExitSuccess      -- Erstellt ein neues Webseiten-Projekt mit dem default Template.
       ["create",site,template] -> create site template >> exitWith ExitSuccess       -- Erstellt ein neues Webseiten-Projekt mit dem angegebenen Template.
       ["settemplate",template] -> setTemplate template >> exitWith ExitSuccess       -- Ändert das Template auf das angegebene Template.
       ["addpage",name]         -> addPage name >> exitWith ExitSuccess               -- Fügt eine neue Seite hinzu (.page und .conf Datei).
       ["build"]                -> buildSite >> exitWith ExitSuccess                  -- Erstellt die HTML-Dateien.
       []                       -> buildSite >> exitWith ExitSuccess                  -- Erstellt die HTML-Dateien.

-- Erstellt die Ordnerstruktur für ein neues Webseiten-Projekt und kopiert alle notwendigen Dateien.
-- Falls es schon eine Webseite mit dem Namen gibt, wird die Funktion abgebrochen.
-- Die Funktion wird ebenfalls abgebrochen, wenn das angegebene Template nicht vorhanden ist.
create :: FilePath -> String -> IO ()
create site template = do
  existsDir <- doesDirectoryExist site
  when existsDir $ do
    hPutStrLn stderr $ "Eine Webite mit dem Namen " ++ site ++ " existiert bereits."
    exitWith $ ExitFailure 2
  templateDir <- getDataFileName "templates"
  existsTemplate <- doesDirectoryExist (templateDir </> template)
  when (existsTemplate == False) $ do
    hPutStrLn stderr $ "Eine Template mit dem Namen " ++ template ++ " existiert nicht."
    exitWith $ ExitFailure 3
  createDirectoryIfMissing True site
  createDirectoryIfMissing True (site</>"output")
  templateContent <- liftM (filter (/=".") . map (makeRelative (templateDir </> template))) $ getDirectoryContentsRecursive (templateDir </> template)
  forM_ templateContent $ \file -> do
    let dest = site </> "src" </> file
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile ((templateDir </> template) </> file) dest
  emptyPage <- liftM (filter (/=".") . map (makeRelative (templateDir </> "emptyPage"))) $ getDirectoryContentsRecursive (templateDir </> "emptyPage")
  forM_ emptyPage $ \file -> do
    let dest = site </> "src" </> file
    createDirectoryIfMissing True $ takeDirectory dest
    copyFile ((templateDir </> "emptyPage") </> file) dest
  writeFile (site</>"config.conf") "author: Dein Name\nyear: 2012\nwebsite_title: Titel der Webseite\nwebsite_slogan: Slogan der Webseite\ndescription: Beschreibung\nkeywords: Keywoerter"