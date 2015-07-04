import Hackern.SharedDB
import Hackern.Interactive.Repl
import Hackern.FS.API
import Hackern.Network.Server
import Hackern.Network.Send

main :: IO ()
main = do
  xs <- initXenStore
  con <- initXenConsole

  withServer xs con $ \t -> do
    writeDebugConsole "Server Daemon Launching..."
    serverDaemon con transport t
    withFS xs con $ \rootDir fsState -> do

        let shellState = ShellState_ {
            _here = rootDir,
            _xs   = xs,
            _con  = con
        }

        runShell shellState fsState
