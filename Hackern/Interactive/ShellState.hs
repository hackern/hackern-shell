module Hackern.Interactive.ShellState where
import Hypervisor.XenStore
import Hypervisor.Console
import Prelude hiding (getLine)

data ShellState = ShellState_ {
  _here  :: FilePath,
  _xs    :: XenStore,
  _con   :: Console
}

getLine con = do
  nextC <- readConsole con 1
  writeConsole con nextC
  case nextC of
    "\r" -> writeConsole con "\n" >> return ""
    [x]  -> (x:) `fmap` getLine con
    _    -> fail "More than one character back?"
