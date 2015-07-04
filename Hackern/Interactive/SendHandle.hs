module Hackern.Interactive.SendHandle where

import Hackern.Interactive.ShellState
import Hackern.Network.Send
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader

handleDiscover shellState@(ShellState_ here xs con) = do
  peers <- lift $ discoverPeers xs
  lift $ writeConsole con $ "Peers: " ++ show peers
  return shellState
