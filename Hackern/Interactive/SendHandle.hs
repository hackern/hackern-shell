module Hackern.Interactive.SendHandle where

import Hackern.Network.Send
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader

handleDiscover xs con loop = do
  peers <- lift $ discoverPeers xs
  lift $ writeConsole con $ "Peers: " ++ show peers
  loop
