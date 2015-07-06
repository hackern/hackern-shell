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

data ConnectionState = ConnectionState_ {
    otherId     :: Maybe DomId,
    sendHandler :: Maybe (String -> IO ()),
    peers       :: Maybe [DomId]
}

initConnState = ConnectionState_ Nothing Nothing Nothing

discoverPeers :: XenStore -> IO [DomId]
discoverPeers xs = waitForDoms xs 1

connectPeer :: Transport -> XenStore -> DomId -> IO (String -> IO (Either (TransportError SendErrorCode) ()))
connectPeer transport xs dom = do
  xs <- initXenStore

  Right endpoint <- newEndPoint transport

  let serverAddr = encodeEndPointAddress dom 0

  Right conn <- connect endpoint serverAddr ReliableOrdered defaultConnectHints
  let sendHandler = \s -> send conn [BSC.pack s]
  return sendHandler

closePeer :: Console -> IO ()
closePeer console = console "closing...\n"
