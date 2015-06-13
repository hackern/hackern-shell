{-# LANGUAGE OverloadedStrings #-}
module Hackern.FS.DiskBD where

import System.Device.BlockDevice
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import XenDevice.Disk

{-
  You need to wait for a while after launch the file system,
  or maybe some tapping or entering etc.
-}

-- Implement the Device Interface
newDiskBlockDevice :: Disk -> IO (Maybe (BlockDevice IO))
newDiskBlockDevice disk = return $! Just BlockDevice {
        bdBlockSize  = fromIntegral $ diskSectorSize disk
      , bdNumBlocks  = fromIntegral $ diskSectors disk
      , bdReadBlock  = \sector -> do
            bl <- readDisk disk (fromIntegral $ diskSectorSize disk) (fromIntegral sector)
            return $ bLtoBS bl
      , bdWriteBlock = \sector bs -> writeDisk disk (bStoBL bs) $ fromIntegral sector
      , bdFlush      = return () -- `flushDiskCaches` is not actaully usable
      , bdShutdown   = return () -- not found in XenDevice.Disk
      }

bLtoBS :: BL.ByteString -> BS.ByteString
bLtoBS = BS.concat . BL.toChunks
bStoBL :: BS.ByteString -> BL.ByteString
bStoBL bs = BL.fromChunks [bs]

