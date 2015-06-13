module Hackern.System.Meta where

import Hypervisor.Console
import Hypervisor.Debug
import Hypervisor.XenStore


data Meta = Meta_ {
    _xStore   :: XenStore,
    _xConsole :: String -> IO (),
    _xDebug   :: String -> IO ()
  }

initMeta = do
  con <- initXenConsole
  xs  <- initXenStore
  return Meta_ {
      _xStore   = xs,
      _xConsole = \s -> writeConsole con $ "[CONSOLE]" ++ s ++ "\n",
      _xDebug   = \s -> writeDebugConsole $ "[DEBUG]" ++ s ++ "\n"
    }
