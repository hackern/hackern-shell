module Hackern.Interactive.SendHandle where

import Hackern.Interactive.ShellState
import Hackern.Network.Send
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader


appConnect shellState@(ShellState_ here xs con) maybeConn = do
  let console = lift . writeConsole con str

  console (here ++ "Connect Application: exit, connect\n")
  case maybeConn of
    Nothing          -> console "currently not connected\n"
    Just (id, _, _)  -> console "currently connected with " ++ id ++ "\n"

  peers <- lift $ discoverPeers xs
  console $ "Available Peers: " ++ show peers ++ "\n"

  inquery <- lift $ getLine con

  case words inquery of

    ("exit":_)       -> return ()

    ("connect":x:_)  -> if x `elem` peers then do
        (t, sender) <- connectPeer xs x
        writeConsole con $ "Connecting to " ++ x ++ "...\n"
        appConnect shellState $ Just (x, t, sender)
      else do
        writeConsole con $ "Failed: " ++ x ++ " doesn't exist!\n"
        appConnect shellState Nothing

    ("send":msg:_)   -> case maybeConn of
      Nothing -> do
        writeConsole con $ "Not connected!\n"
        appConnect shellState Nothing
      Just (_, _, sender) -> do
        sender msg
        appConnect shellState maybeConn

    ("close":_)     -> case maybeConn of
      Nothing -> do
        writeConsole con $ "Not connected!\n"
        appConnect shellState Nothing
      Just (_, t, _) -> do
        closePeer t
        appConnect shellState Nothing

    _ -> do
      console "Unrecognized command\n"
      loop shellState
