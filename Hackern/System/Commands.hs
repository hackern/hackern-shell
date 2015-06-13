module Hackern.System.Commands where
import Hypervisor.Shutdown
import Hackern.System.Meta
import Control.Concurrent

poweroff :: Meta -> IO ()
poweroff (Meta_ xs _ _) = onPoweroff xs $ putStrLn "I'm shutting down! Bye."
onReboot (Meta_ xs _ _) = onReboot xs $ putStrLn "I'm trying to reboot! Bye."
exit                    = exitSuccess
idleSeconds secs        = threadDelay (secs * 1000000)
