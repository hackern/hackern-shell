module Hackern.Interactive.SendHandle where

import Hackern.Interactive.ShellState
import Hackern.Network.Send
import Hypervisor.DomainInfo (DomId(..))
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader
import Prelude hiding (getLine)

appConnect shellState@(ShellState_ here xs con) maybeConn = do
  let console = lift . writeConsole con
  console "Valid commands: exit, connect, send\n"
  case maybeConn of
    Nothing       -> do peers <- lift $ discoverPeers xs
                        console $ "Available Peers: " ++ show peers ++ "\n"

    Just (id, _)  -> console $ "\tStatus: Connected with " ++ id ++ "\n"


  console $ "> "
  inquery <- lift $ getLine con

  case words inquery of

    ("exit":_)       -> return ()

    ("connect":x:_)  -> do -- if (read x :: DomId) `elem` peers then do
        sender <- lift $ connectPeer xs (read x :: DomId)
        console $ "Connecting to " ++ x ++ "...\n"
        appConnect shellState $ Just (x, sender)
{-      else do
        console $ "Failed: " ++ x ++ " doesn't exist!\n"
        appConnect shellState Nothing -}

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
