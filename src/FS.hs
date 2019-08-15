{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FS (bbMkFuseOps) where

import           Control.Exception                            (IOException,
                                                               bracket, try)
import           Control.Monad                                (void)

import           Data.ByteString                              (ByteString)

import           Foreign.C.Error                              (eNOSYS, eOK)

import qualified System.Posix.Directory                       as Posix
import qualified System.Posix.Files                           as Posix
import qualified System.Posix.IO                              as Posix
import qualified System.Posix.Types                           as Posix

import qualified "unix-bytestring" System.Posix.IO.ByteString as Posix

import qualified System.Fuse                                  as Fuse

newtype BaseDir = BaseDir FilePath

absPath :: BaseDir -> FilePath -> FilePath
absPath (BaseDir baseDir) relPath = baseDir ++ relPath

bbMkFuseOps :: FilePath -> Fuse.FuseOperations Posix.Fd
bbMkFuseOps baseDirPath = Fuse.FuseOperations
  { Fuse.fuseGetFileStat = bbGetattr baseDir
  , Fuse.fuseReadSymbolicLink = bbReadlink baseDir
  , Fuse.fuseCreateDevice = bbMknod baseDir
  , Fuse.fuseCreateDirectory = bbMkdir baseDir
  , Fuse.fuseRemoveLink = bbUnlink baseDir
  , Fuse.fuseRemoveDirectory = bbRmdir baseDir
  , Fuse.fuseCreateSymbolicLink = bbSymlink baseDir
  , Fuse.fuseRename = bbRename baseDir
  , Fuse.fuseCreateLink = bbLink baseDir
  , Fuse.fuseSetFileMode = bbChmod baseDir
  , Fuse.fuseSetOwnerAndGroup = bbChown baseDir
  , Fuse.fuseSetFileSize = bbTruncate baseDir
  , Fuse.fuseSetFileTimes = bbUtime baseDir
  , Fuse.fuseOpen = bbOpen baseDir
  , Fuse.fuseRead = bbRead
  , Fuse.fuseWrite = bbWrite
  , Fuse.fuseGetFileSystemStats = bbStatfs
  , Fuse.fuseFlush = bbFlush
  , Fuse.fuseRelease = bbRelease
  , Fuse.fuseSynchronizeFile = bbFsync
  , Fuse.fuseOpenDirectory = bbOpendir
  , Fuse.fuseReadDirectory = bbReaddir baseDir
  , Fuse.fuseReleaseDirectory = bbReleasedir
  , Fuse.fuseSynchronizeDirectory = bbFsyncdir
  , Fuse.fuseAccess = bbAccess
  , Fuse.fuseInit = bbInit
  , Fuse.fuseDestroy = bbDestroy
  }
 where
  baseDir = BaseDir baseDirPath

getEntryType :: Posix.FileStatus -> Fuse.EntryType
getEntryType fileStatus
  | Posix.isBlockDevice fileStatus = Fuse.BlockSpecial
  | Posix.isCharacterDevice fileStatus = Fuse.CharacterSpecial
  | Posix.isNamedPipe fileStatus = Fuse.NamedPipe
  | Posix.isRegularFile fileStatus = Fuse.RegularFile
  | Posix.isDirectory fileStatus = Fuse.Directory
  | Posix.isSymbolicLink fileStatus = Fuse.SymbolicLink
  | Posix.isSocket fileStatus = Fuse.Socket
  | otherwise = Fuse.Unknown

getFileStat :: Posix.FileStatus -> Fuse.FileStat
getFileStat fileStatus = Fuse.FileStat
  { Fuse.statEntryType = getEntryType fileStatus
  , Fuse.statFileMode = Posix.fileMode fileStatus
  , Fuse.statLinkCount = Posix.linkCount fileStatus
  , Fuse.statFileOwner = Posix.fileOwner fileStatus
  , Fuse.statFileGroup = Posix.fileGroup fileStatus
  , Fuse.statSpecialDeviceID = Posix.specialDeviceID fileStatus
  , Fuse.statFileSize = Posix.fileSize fileStatus
  , Fuse.statBlocks = statBlocks
  , Fuse.statAccessTime = Posix.accessTime fileStatus
  , Fuse.statModificationTime = Posix.modificationTime fileStatus
  , Fuse.statStatusChangeTime = Posix.statusChangeTime fileStatus }
 where
  fileSize = Posix.fileSize fileStatus
  statBlocks = ceiling $ (fromIntegral fileSize :: Double) / 512

_tryFuse :: IO () -> IO Fuse.Errno
_tryFuse f = do
  eitherValue <- try f
  case eitherValue of
    Left (_ :: IOException) -> Fuse.getErrno
    Right _                 -> pure eOK

tryFuse :: IO a -> IO (Either Fuse.Errno a)
tryFuse f = do
  eitherValue <- try f
  case eitherValue of
    Left (_ :: IOException) -> Left <$> Fuse.getErrno
    Right value             -> pure $ Right value

bbGetattr :: BaseDir -> FilePath -> IO (Either Fuse.Errno Fuse.FileStat)
bbGetattr baseDir relPath = tryFuse $ getFileStat <$> Posix.getSymbolicLinkStatus filePath
 where filePath = absPath baseDir relPath

bbReadlink :: BaseDir -> FilePath -> IO (Either Fuse.Errno FilePath)
bbReadlink baseDir relPath = tryFuse $ Posix.readSymbolicLink filePath
 where filePath = absPath baseDir relPath

bbMknod :: BaseDir -> FilePath -> Fuse.EntryType -> Posix.FileMode -> Posix.DeviceID -> IO Fuse.Errno
bbMknod baseDir relPath _ fileMode deviceId = _tryFuse $ Posix.createDevice filePath fileMode deviceId
 where filePath = absPath baseDir relPath

bbMkdir :: BaseDir -> FilePath -> Posix.FileMode -> IO Fuse.Errno
bbMkdir baseDir relPath fileMode = _tryFuse $ Posix.createDirectory filePath fileMode
 where filePath = absPath baseDir relPath

bbUnlink :: BaseDir -> FilePath -> IO Fuse.Errno
bbUnlink baseDir relPath = _tryFuse $ Posix.removeLink filePath
 where filePath = absPath baseDir relPath

bbRmdir :: BaseDir -> FilePath -> IO Fuse.Errno
bbRmdir baseDir relPath = _tryFuse $ Posix.removeDirectory filePath
 where filePath = absPath baseDir relPath

bbSymlink :: BaseDir -> FilePath -> FilePath -> IO Fuse.Errno
bbSymlink baseDir filePath1 relPath2 = _tryFuse $ Posix.createSymbolicLink filePath1 filePath2
 where filePath2 = absPath baseDir relPath2

bbRename :: BaseDir -> FilePath -> FilePath -> IO Fuse.Errno
bbRename baseDir relPath1 relPath2 = _tryFuse $ Posix.rename filePath1 filePath2
 where
  filePath1 = absPath baseDir relPath1
  filePath2 = absPath baseDir relPath2

bbLink :: BaseDir -> FilePath -> FilePath -> IO Fuse.Errno
bbLink baseDir relPath1 relPath2 = _tryFuse $ Posix.createLink filePath1 filePath2
 where
  filePath1 = absPath baseDir relPath1
  filePath2 = absPath baseDir relPath2

bbChmod :: BaseDir -> FilePath -> Posix.FileMode -> IO Fuse.Errno
bbChmod baseDir relPath fileMode = _tryFuse $ Posix.setFileMode filePath fileMode
 where filePath = absPath baseDir relPath

bbChown :: BaseDir -> FilePath -> Posix.UserID -> Posix.GroupID -> IO Fuse.Errno
bbChown baseDir relPath userId groupId = _tryFuse $ Posix.setOwnerAndGroup filePath userId groupId
 where filePath = absPath baseDir relPath

bbTruncate :: BaseDir -> FilePath -> Posix.FileOffset -> IO Fuse.Errno
bbTruncate baseDir relPath fileOffset = _tryFuse $ Posix.setFileSize filePath fileOffset
 where filePath = absPath baseDir relPath

bbUtime :: BaseDir -> FilePath -> Posix.EpochTime -> Posix.EpochTime -> IO Fuse.Errno
bbUtime baseDir relPath aTime mTime = _tryFuse $ Posix.setFileTimes filePath aTime mTime
 where filePath = absPath baseDir relPath

bbOpen :: BaseDir -> FilePath -> Fuse.OpenMode -> Fuse.OpenFileFlags -> IO (Either Fuse.Errno Posix.Fd)
bbOpen baseDir relPath openMode openFlags = tryFuse $ Posix.openFd filePath openMode Nothing openFlags
 where filePath = absPath baseDir relPath

bbRead :: FilePath -> Posix.Fd -> Posix.ByteCount -> Posix.FileOffset -> IO (Either Fuse.Errno ByteString)
bbRead _ fd byteCount fileOffset = tryFuse $ Posix.fdPread fd byteCount fileOffset

bbWrite :: FilePath -> Posix.Fd -> ByteString -> Posix.FileOffset -> IO (Either Fuse.Errno Posix.ByteCount)
bbWrite _ fd byteString fileOffset = tryFuse $ Posix.fdPwrite fd byteString fileOffset

bbStatfs :: FilePath -> IO (Either Fuse.Errno Fuse.FileSystemStats)
bbStatfs _ = pure $ Left eNOSYS

bbFlush :: FilePath -> Posix.Fd -> IO Fuse.Errno
bbFlush _ _ = pure eOK

bbRelease :: FilePath -> Posix.Fd -> IO ()
bbRelease _ fd = void . _tryFuse $ Posix.closeFd fd

bbFsync :: FilePath -> Fuse.SyncType -> IO Fuse.Errno
bbFsync _ _ = pure eNOSYS

bbOpendir :: FilePath -> IO Fuse.Errno
bbOpendir _ = pure eOK

bbReaddir :: BaseDir -> FilePath -> IO (Either Fuse.Errno [(FilePath, Fuse.FileStat)])
bbReaddir baseDir relPath = tryFuse $ bracket
  (Posix.openDirStream filePath)
  Posix.closeDirStream
  readDir
 where
  filePath = absPath baseDir relPath
  readDir dirStream = Posix.readDirStream dirStream >>= \case
    "" -> pure []
    dirFilePath -> do
      dirFileStatus <- Posix.getSymbolicLinkStatus filePath
      let dirFileStat = getFileStat dirFileStatus
      dirFiles <- readDir dirStream
      pure $ (dirFilePath, dirFileStat) : dirFiles

bbReleasedir :: FilePath -> IO Fuse.Errno
bbReleasedir _ = pure eOK

bbFsyncdir :: FilePath -> Fuse.SyncType -> IO Fuse.Errno
bbFsyncdir _ _ = pure eNOSYS

bbAccess :: FilePath -> Int -> IO Fuse.Errno
bbAccess _ _ = pure eNOSYS

bbInit :: IO ()
bbInit = pure ()

bbDestroy :: IO ()
bbDestroy = pure ()
