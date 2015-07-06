module Hackern.Interactive.SendHandle where

import Hackern.Interactive.ShellState
import Hackern.Network.Send
import Hypervisor.DomainInfo (DomId(..))
import Control.Monad
import Hypervisor.Console
import Control.Monad.Reader
import Prelude hiding (getLine)

appConnect shellState@(ShellState_ here xs con transport)
           connState@(ConnectionState_ mId, mSend, mPeers) = do

  let console = lift . writeConsole con
  console "Valid commands: exit, connect [DomId], send [String]\n"
  case (mPeers, mId) of
    (Nothing, _) -> do
      peers <- lift $ discoverPeers xs
      console $ "Available Peers: " ++ show peers ++ "\n"

    (_, Just id) -> console $ "Status: Connected with " ++ id ++ "\n"


  console $ "> "
  inquery <- lift $ getLine con

  case words inquery of

    ("exit":_)       -> return ()

    ("connect":x:_)  -> case mPeers of
      Just peers -> if (read x :: DomId) `elem` peers then do
          send <- lift $ connectPeer transport xs (read x :: DomId)
          console $ "Connecting to " ++ x ++ "...\n"
          appConnect shellState $ ConnectionState_ mId (Just send) mPeers
        else do
          console $ "Failed: " ++ x ++ " doesn't exist!\n"
          appConnect shellState connState
      Nothing -> console "Peers not detected"

    ("send":msg:_)   -> case mSend of
      Nothing -> do
        console $ "Not connected!\n"
        appConnect shellState connState
      Just send -> do
        lift $ send msg
        appConnect shellState connState

    _ -> do
      console "Unrecognized command\n"
      appConnect shellState connState
