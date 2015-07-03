import Hackern.SharedDB
import Hackern.Interactive.Repl
import Hackern.FS.Utils
import Hackern.Network.Server
import Hackern.Network.Send
import Hackern.System.Config

main :: IO ()
main = do
  config@(xs, con, debug) <- initConfig
  withServer config $ \t -> do
    debug "Server Daemon Running..."
    serverDaemon config t
    launchFS config repl

 
