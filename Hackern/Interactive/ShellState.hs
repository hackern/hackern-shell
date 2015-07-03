module Hackern.Interactive.ShellState where
import Hypervisor.XenStore
import Hypervisor.Console

data ShellState = ShellState_ {
  _here  :: FilePath,
  _xs    :: XenStore,
  _con   :: Console
}


