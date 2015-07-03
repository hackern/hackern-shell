module Hackern.Network.Server (
  withServer
, serverDaemon
) where

import Network.Transport
import Network.Transport.IVC

import Hypervisor.XenStore
import Hypervisor.Debug
import Hypervisor.Console
import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8 as BSC

withServer (xs, _, _) f = do
  t <- createTransport xs
  case t of
    Left exp        -> print $ "can init server since: " ++ show exp
    Right transport -> f transport >> closeTransport transport

serverDaemon (xs, con, debug) transport = do

  Right endpoint <- newEndPoint transport

  void . forkIO . forever $ do
    event <- receive endpoint
    case event of
      ConnectionOpened _ _ addr ->
        writeConsole con $ "connection from " ++ show addr ++ "\n"
      Received _ bss ->
        forM_ bss $ \bs ->
          writeConsole con $ "Data Received" ++ show (BSC.unpack bs) ++ "\n"
