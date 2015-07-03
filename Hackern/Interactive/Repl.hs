module Hackern.Interactive.Repl where
-- import Data.Word
import Hackern.Interactive.FSHandle
import Hypervisor.XenStore
import Control.Monad.Reader
import Hypervisor.Debug
import Control.Concurrent
import Halfs.Monad
-- import Control.Exception
-- import Control.Monad
import Hypervisor.Console
-- import Hypervisor.ErrorCodes

import Prelude hiding (getLine)

-- The REPL shell loop
repl xs con debug here fsState = do
  let console str = writeConsole con $ str ++ "\n"
  me <- xsGetDomId xs
  console $ "Hello! This is an interactive Unix-like file-system shell for " ++ show me ++ "\n"
  console $ "Valid commands: quit, ls, cd, mkdir\n"
  debug "Starting interaction loop!\n"
  info <- runHalfs fsState $ loop con here
  return ()

loop con here  = do
  lift $ writeConsole con (here ++ "> ")
  inquery <- lift $ getLine con
  case words inquery of
    ("quit":_)    -> return ()
    ("ls"  :_)    -> handleLs here con loop
    ("cd"  :x:_)  -> handleCd here con x loop
    ("mkdir":x:_) -> handleMkdir here x con loop
    _ -> do
      lift $ writeConsole con "Unrecognized command\n"
      loop con here

getLine con = do
  nextC <- readConsole con 1
  writeConsole con nextC
  case nextC of
    "\r" -> writeConsole con "\n" >> return ""
    [x]  -> (x:) `fmap` getLine con
    _    -> fail "More than one character back?"


