module Hackern.Interactive.Shell where

import Hackern.Interactive.SendHandle
import Hackern.Interactive.FSHandle
import Hackern.Interactive.ShellState
import Hackern.Storage.KV

import Hypervisor.Console
import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad.Reader
import Control.Concurrent

import Halfs.Monad
import Prelude hiding (getLine)

-- The REPL shell loop
runShell shellState@(ShellState_ here xs con dev) fsState = do
  let console str = writeConsole con $ str ++ "\n"
  me <- xsGetDomId xs
  console $ "Welcome to Hackern system! I am " ++ show me ++ "\n"
  console $ "Valid commands: quit, ls, cd, mkdir, talk\n"
  _ <- runHalfs fsState (loop shellState)
  return ()

loop shellState@(ShellState_ here xs con dev) = do
  lift $ writeConsole con (here ++ "> ")
  inquery <- lift $ getLine con

  let dispatch f = f shellState >>= loop

  case words inquery of
    ("quit":_)       -> return ()
    ("ls"  :_)       -> dispatch handleLs
    ("cd"  :x:_)     -> dispatch $ handleCd x
    ("mkdir":x:_)    -> dispatch $ handleMkdir x
    ("talk":_)       -> appConnect shellState Nothing >> loop shellState
    ("read":x:_)     -> do
       maybeV <- lift $ load dev x
       case maybeV of
         Just v  -> lift $ writeConsole con $ "Value: " ++ v ++ "\n"
         Nothing -> lift $ writeConsole con $ "Invalid Key\n"
       loop shellState
    ("write":k:v:_)  -> lift (store dev k v) >> loop shellState
    _ -> do
      lift $ writeConsole con "Unrecognized command\n"
      loop shellState

