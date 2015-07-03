module Hackern.System.Config where

import Hypervisor.XenStore
import Hypervisor.Console
import Hypervisor.Debug


initConfig :: IO (XenStore, Console, String -> IO())
initConfig = do
  xs <- initXenStore
  con <- initXenConsole
  let debug str = writeDebugConsole $ str ++ "\n"
  return (xs, con, debug)
