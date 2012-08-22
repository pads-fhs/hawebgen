module StatiGen.Types where

-- Datentyp Site
-- Hier werden die Metainformationen aus der allgemeinen Konfigurationsdatei zwischen gespeichert
data Site = Site {
    siteAuthor      :: String
  , siteDate        :: String  
  , siteTitle       :: String
  , siteSlogan      :: String
  , siteDescription :: String
  , siteKeywords    :: String
  } deriving (Show)

-- Datentyp Page
-- Hier werden die Metainformationen einer einzelnen Seite, sowie deren Content gespeichert.
-- Außerdem auch Informationen über den Navigationseintrag
data Page = Page {
    pageTitle     :: String
  , pageInMenu    :: Bool
  , pageNav       :: NavEntry
  , pageContent   :: FilePath
  } deriving (Show)

-- Datentyp NavEntry
-- Ein Eintrag in der Navigation.
data NavEntry = NavEntry {
    navOrder     :: Integer
  , navTitle     :: String
  , navUrl       :: String
  } deriving (Show, Eq, Ord)