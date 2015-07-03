module Hackern.SharedDB where

import Control.Concurrent
import Hypervisor.XenStore
import Hypervisor.ErrorCodes
import Control.Exception
import System.FilePath (FilePath, (</>))
import Prelude hiding (read)
import Control.Applicative

{-
  Use XenUtil to generate a playground first
-}

data Database = Database_ {
    read   :: FilePath -> IO String,
    update :: FilePath -> String -> IO (),
    delete :: FilePath -> IO (),
    withDB :: (XenStore -> FilePath -> IO ()) -> IO ()
  }

createDB xs path = do
  me <- xsGetDomId xs
  removePath xs path
  xsMakeDirectory xs path
  xsSetPermissions xs path [ReadWritePerm me]
  return Database_ {
    read   = xsRead xs,
    update = xsWrite xs,
    delete = xsRemove xs,
    withDB = \f -> f xs path
  }

removeDB :: Database -> IO ()
removeDB db = (withDB db) removePath

removePath :: XenStore -> String -> IO ()
removePath xs str = do catch remSubItems onECContinue
                       catch remItem     onECContinue
  where
    remSubItems = mapM_ (removePath xs) =<< xsDirectory xs str
    remItem     = removePath xs str
    onECContinue :: ErrorCode -> IO ()
    onECContinue _ = return ()

waitForKey :: Database -> String -> IO String
waitForKey db key = do
  err <- catch (Right <$> read db key) leftError
  case err of
    Left _    -> threadDelay 100000 >> waitForKey db key
    Right res -> return res
 where
  leftError :: ErrorCode -> IO (Either ErrorCode String)
  leftError = return . Left

listKeys :: XenStore -> FilePath -> IO [FilePath]
listKeys xs here = filter (/= "") `fmap` xsDirectory xs here

