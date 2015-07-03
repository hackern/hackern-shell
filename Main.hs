import Hackern.SharedDB
import Hackern.Interactive.Shell
import Hackern.FS.API
import Hackern.Network.Server
import Hypervisor.XenStore
import Hypervisor.Debug
import Hypervisor.Console
import Hackern.Interactive.ShellState
import Hackern.Network.Send

main :: IO ()
main = do
  xs <- initXenStore
  con <- initXenConsole

  withServer xs con $ \t -> do
    writeDebugConsole "Server Daemon Launching..."
    serverDaemon con t
    withFS xs con $ \rootDir fsState -> do

        let shellState = ShellState_ {
            _here = rootDir,
            _xs   = xs,
            _con  = con
        }

        runShell shellState fsState
