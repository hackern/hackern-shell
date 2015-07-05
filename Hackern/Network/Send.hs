module Hackern.Network.Send (
  discoverPeers
, connectPeer
, closePeer
) where

import Network.Transport
import Network.Transport.IVC

import Hypervisor.DomainInfo (DomId(..))
import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BSC

discoverPeers xs = waitForDoms xs 1

connectPeer :: XenStore -> DomId -> IO (String -> IO (Either (TransportError SendErrorCode) ()))
connectPeer xs dom = do
  xs <- initXenStore

  Right transport <- createTransport xs
  Right endpoint <- newEndPoint transport

  let serverAddr = encodeEndPointAddress dom 0

  Right conn <- connect endpoint serverAddr ReliableOrdered defaultConnectHints
  let sender = \s -> send conn [BSC.pack s]
  return sender

closePeer console = console "closing...\n"
