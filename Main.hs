import Hackern.SharedDB
import Hackern.Interactive.Shell
import Hackern.FS.API
import Hackern.Storage.API
import Hackern.Network.Server
import Hackern.Interactive.ShellState
import Hackern.Network.Send

import Hypervisor.XenStore
import Hypervisor.Debug
import Hypervisor.Console


main :: IO ()
main = do
  xs <- initXenStore
  con <- initXenConsole

  withServer xs con $ \t -> do
    writeDebugConsole "Server Daemon Launching..."
    serverDaemon con t
    withFS xs con $ \rootDir fsState ->
        withStorage $ \dev -> do
          let shellState = ShellState_ {
            _here = rootDir,
            _xs   = xs,
            _con  = con,
            _dev  = dev
          }

          runShell shellState fsState
