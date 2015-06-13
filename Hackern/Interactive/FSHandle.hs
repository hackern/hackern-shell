module Hackern.Interactive.FSHandle where
import Hackern.FS.Utils
import Control.Monad
import Hypervisor.Console
import System.FilePath((</>), takeDirectory)
import Halfs.CoreAPI
import Halfs.Monad

handleLs here con loop = do
  handle <- openDir here
  dirInfo <- readDir handle
  lift $ writeConsole con $ printDir dirInfo ++ "\n"
  loop con here

handleCd here con x loop  = case x of
  ".." -> loop con (takeDirectory here)
  d    -> do
    handle <- openDir here
    dirInfo <- readDir handle
    if (filter (== d) $ map fst dirInfo) /= [] then
      loop con (here </> d)
      else do
        lift $ writeConsole con "No such directory\n"
        loop con here

handleMkdir here x con loop = do
  if x /= ".." then mkdir (here </> x) defaultPerm
    else lift $ writeConsole con "Invalid directory name\n"
  loop con here

printDir [] = ""
printDir (x:[]) = fst x
printDir (x:xs) = fst x ++ "\t" ++ printDir xs
