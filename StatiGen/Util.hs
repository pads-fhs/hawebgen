module StatiGen.Util where

import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Control.Monad

-- Durchsucht einen Ordner und gibt eine Liste aller Unterordner und Dateien zur�ck.
-- Es werden auch Unterordner der Unterordner ber�cksichtigt.
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       contents <- getDirectoryContents path
       let contents' = map (path </>) $ filter (`notElem` ["..","."]) contents
       children <- mapM getDirectoryContentsRecursive contents'
       return (concat children)
     else return [path]

-- Sortiert eine Liste
quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = quickSort kleinergl ++ [x] ++ quickSort groesser
                   where
                     kleinergl = [y | y <- xs, y <= x]
                     groesser  = [y | y <- xs, y > x]

-- Ersetzt in einem �bergebenen Text einen bestimmten String mit einem neuen String.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

-- Gibt das erste Element einer Liste zur�ck.
getFirstElement :: Monad m => [a] -> m a
getFirstElement (x:xs) = return x

-- Liefert das Template als String zur�ck. Konnte das Template nicht gefunden werden, wird das Programm
-- mit einer Fehlermeldung abgebrochen.
getTemplate :: IO String
getTemplate = do
  existsTemplate <- doesFileExist ("src" </> "default.template")
  when (existsTemplate == False) $ do
    hPutStrLn stderr $ "Die Seite konnte nicht gebaut werden, da kein Template vorhanden ist!"
    exitWith $ ExitFailure 4
  template <- readFile ("src" </> "default.template")
  return template

-- �berpr�ft ob sich der Benutzer in einem Webseiten-Projekt Ordner aufh�lt. Ist die Datei config.conf
-- und der Ordner src nicht vorhanden, ist dies nicht der Fall und es wird False zur�ckgegeben. Ansonsten True.
checkWebsite :: IO Bool
checkWebsite = do
  let check = False
  exsitsConfig <- doesFileExist "config.conf"
  if exsitsConfig
     then do
       existsSrc <- doesDirectoryExist "src"
       return existsSrc
     else return False