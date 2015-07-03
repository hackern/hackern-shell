module Hackern.Network.Send (
  discoverPeers
, connectPeer
, closePeer
) where

import Network.Transport
import Network.Transport.IVC

import Hypervisor.XenStore
import Hypervisor.Debug

import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as BSC

discoverPeers xs = waitForDoms xs 1

connectPeer xs dom = do
  xs <- initXenStore

  Right transport <- createTransport xs
  Right endpoint <- newEndPoint transport

  let serverAddr = encodeEndPointAddress dom 0

  Right conn <- connect endpoint serverAddr ReliableOrdered defaultConnectHints

  return (transport, send conn)

--    send conn [BSC.pack (show (2*i)), BSC.pack (show (2*i + 1))]

closePeer transport = closeTransport transport
