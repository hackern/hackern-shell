module Hackern.SharedDB where
import Hackern.System.Meta
import Control.Concurrent
import Hypervisor.XenStore
import Hypervisor.ErrorCodes
import Control.Exception
import System.FilePath (FilePath, (</>))
import Control.Applicative

{-
  Use XenUtil to generate a playground first
-}

newTable :: Meta -> FilePath -> IO ()
newTable meta@(Meta_ xs xc xd) path = do
  me <- xsGetDomId xs
  removeTable meta path
  xsMakeDirectory xs path
  xsSetPermissions xs path [ReadWritePerm me]

removeEntry (Meta_ xs _ _) path = xsRemove xs path
readEntry   (Meta_ xs _ _) path = xsRead xs path
writeEntry  (Meta_ xs _ _) path value = xsWrite xs path value

waitForKey :: Meta -> String -> IO String
waitForKey meta key = do
  err <- catch (Right <$> readEntry meta key) leftError
  case err of
    Left _    -> threadDelay 100000 >> waitForKey meta key
    Right res -> return res
 where
  leftError :: ErrorCode -> IO (Either ErrorCode String)
  leftError = return . Left

listKeys :: XenStore -> FilePath -> IO [FilePath]
listKeys xs here = filter (/= "") `fmap` xsDirectory xs here

removeTable :: Meta -> String -> IO ()
removeTable meta@(Meta_ xs _ _) str = do catch remSubItems onECContinue
                                         catch remItem     onECContinue
  where
    remSubItems = mapM_ (removeTable meta) =<< xsDirectory xs str
    remItem     = removeEntry meta str
    onECContinue :: ErrorCode -> IO ()
    onECContinue _ = return ()

listValues meta here keys = mapM (readEntry meta) (map (here </>) keys)
