module Hackern.Network.Send (
  discoverPeers
, connectPeer
, closePeer
) where

import Network.Transport
import Network.Transport.IVC

import Hypervisor.DomainInfo (DomId(..))
import Hypervisor.XenStore

import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BSC


type Sender = String -> IO (Either (TransportError SendErrorCode) ())

discoverPeers :: XenStore -> IO [DomId]
discoverPeers xs = waitForDoms xs 1

connectPeer :: XenStore -> DomId -> IO Sender
connectPeer xs dom = do
  xs <- initXenStore

  Right transport <- createTransport xs
  Right endpoint <- newEndPoint transport

  let serverAddr = encodeEndPointAddress dom 0

  Right conn <- connect endpoint serverAddr ReliableOrdered defaultConnectHints
  let sendHandler = \s -> send conn [BSC.pack s]
  return sendHandler

closePeer :: (String -> IO ()) -> IO ()
closePeer console = console "closing...\n"
