module Hackern.FS.Utils where
import Hypervisor.XenStore
import Hypervisor.Console
import XenDevice.Disk
import Halfs.CoreAPI
import Halfs.Types
import Halfs.Monad
import Hackern.FS.DiskBD
import Hypervisor.Debug
import Control.Concurrent (threadDelay)

debug s = writeDebugConsole $ s ++ "\n"
wait = threadDelay 10000

rwx = [Read, Write, Execute]
defaultPerm = FileMode rwx rwx rwx


withFirstDisk xs con cb = do
    diskNames <- listDisks xs
    threadDelay (1000000)
    debug $ "Disks Found: " ++ show diskNames
    case diskNames of
      (diskName:_) -> do -- Use the first available disk
        disk <- openDisk xs diskName
        mdiskBD <- newDiskBlockDevice disk
        case mdiskBD of
          Just diskBD -> do            
            fsState <- mountFS diskBD
            mrootDir <- runHalfs fsState $ do -- Create root and return
              mkdir "/" defaultPerm
              return "/"
            return ()
    
            case mrootDir of
              Right rootDir -> cb xs con rootDir fsState
              Left err -> writeDebugConsole $ "Fail: " ++ show err ++ "\n"

            unmountInfo <- runHalfs fsState unmount
            case unmountInfo of
              Left err -> writeDebugConsole $ "Error in unmounting: " ++ show err
              Right () -> return ()
          Nothing -> writeDebugConsole "Error in initializing disk block device!"
      [] -> writeDebugConsole "No available disks!"


mountFS diskBD = execNoEnv $ mount diskBD 0 0 defaultPerm

execNoEnv :: Monad m => HalfsM b r l m a -> m a
execNoEnv act = do
  runHalfsNoEnv act >>= \ea -> case ea of
    Left e  -> fail $ show e
    Right x -> return x
