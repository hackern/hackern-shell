module Hackern.Interactive.FSHandle where

import Hackern.FS.API
import Control.Monad
import Hypervisor.Console
import System.FilePath((</>), takeDirectory)
import Halfs.CoreAPI
import Halfs.Monad
import Hackern.Interactive.ShellState

handleLs shellState@(ShellState_ here xs con)= do
  handle <- openDir here
  dirInfo <- readDir handle
  lift $ writeConsole con $ printDir dirInfo ++ "\n"
  return shellState

handleCd to shellState@(ShellState_ here xs con) = case to of
  ".." -> return $ ShellState_ (takeDirectory here) xs con
  d    -> do
    handle <- openDir here
    dirInfo <- readDir handle
    if (filter (== d) $ map fst dirInfo) /= [] then
      return $ ShellState_ (here </> d) xs con
      else do
        lift $ writeConsole con "No such directory\n"
        return shellState

handleMkdir path shellState@(ShellState_ here xs con) = do
  if path /= ".."
    then mkdir (here </> path) defaultPerm
    else lift $ writeConsole con "Invalid directory name\n"
  return shellState

printDir [] = ""
printDir (x:[]) = fst x
printDir (x:xs) = fst x ++ "\t" ++ printDir xs
