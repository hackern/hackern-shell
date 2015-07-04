module Hackern.Interactive.ShellState where

data ShellState = ShellState_ {
  _here  :: FilePath,
  _xs    :: XenStore,
  _con   :: Console
}


