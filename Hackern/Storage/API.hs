module Hackern.Storage.API where

import Hackern.Storage.KV
import System.Device.Memory

withStorage f = do
  Just dev <- newMemoryBlockDevice 4096 1048576
  mkKVStorage dev 1048576
  f dev
