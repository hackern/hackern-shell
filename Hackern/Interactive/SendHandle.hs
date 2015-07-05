module Hackern.Interactive.SendHandle where

import Hackern.Interactive.ShellState
import Hackern.Network.Send
import Hypervisor.DomainInfo (DomId(..))
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader
import Prelude hiding (getLine)

appConnect shellState@(ShellState_ here xs con _) maybeConn = do
  let console = lift . writeConsole con
  console "Valid commands: exit, connect, send\n"
  case maybeConn of
    Nothing       -> do peers <- lift $ discoverPeers xs
                        console $ "Available Peers: " ++ show peers ++ "\n"

    Just (id, _)  -> console $ "Status: Connected with " ++ id ++ "\n"


  console $ "> "
  inquery <- lift $ getLine con

  case words inquery of

    ("exit":_)       -> return ()

    ("connect":x:_)  -> do
        sender <- lift $ connectPeer xs (read x :: DomId)
        console $ "Connecting to " ++ x ++ "...\n"
        appConnect shellState $ Just (x, sender)

    ("send":msg:_)   -> case maybeConn of
      Nothing -> do
        console $ "Not connected!\n"
        appConnect shellState Nothing
      Just (_, sender) -> do
        lift $ sender msg
        appConnect shellState maybeConn

    _ -> do
      console "Unrecognized command\n"
      appConnect shellState maybeConn
