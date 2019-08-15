module Main where

import qualified FS

import           System.Environment (getArgs, getProgName)
import qualified System.Fuse        as Fuse

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn "Usage error: base directory required"
    (baseDir:args') -> do
      let fuseOps = FS.bbMkFuseOps baseDir
      Fuse.fuseRun progName args' fuseOps Fuse.defaultExceptionHandler
