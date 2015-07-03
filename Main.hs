import Hackern.SharedDB
import Hackern.Interactive.Repl
import Hackern.FS.Utils
import Hackern.System.Config

main :: IO ()
main = do
  config <- initConfig
  launchFS config repl
  return ()
 
