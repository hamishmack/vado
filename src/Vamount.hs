{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Hamish Mackenzie & Dan Frumin
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
-- Stability   :  Experimental
-- Portability :  Unknown
--
-- | Lets you quickly mount sshfs file systems based on .vadosettings files,
-- for future use with 'vado'
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Exit (exitWith, ExitCode(..))
import System.Process (rawSystem)
import System.Process.Vado (getMountPoint, vado,
                            readSettings, defMountSettings, vamount)
#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#else
import Text.Read (reads)
#endif

#if !MIN_VERSION_base(4,6,0)
-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
#endif    

-- | Main function for vamount
main = do
    args <- getArgs
    case span ("-" `isPrefixOf`) args of
        (sshfsopts,path:rest) -> do
            currentDir <- getCurrentDirectory
            ms <- readSettings
            defSettings <- defMountSettings
            let profileNum 
                  | null rest = 0
                  | otherwise = fromMaybe 0 (readMaybe (head rest))
            when (length ms < profileNum || profileNum < 0) printUsage
            let profile = if profileNum == 0
                          then defSettings
                          else ms !! (profileNum - 1)
            rawSystem "sshfs" (vamount profile path currentDir sshfsopts)
              >>= exitWith
        _ -> printUsage
        
printUsage :: IO ()
printUsage = do
  defSettings <- defMountSettings
  hPutStrLn stderr $
    "Usage vamount [ssh options] remote_path [profile #]\n\n"
    ++ "The remote_path from the remote server specified\n"
    ++ "in the ~/.vadosettings file under number [profile #]\n"
    ++ "will be mounted in the current directory using sshgs\n\n"
    ++ "The ssh options must start with a dash '-'.\n"
    ++ "The profile number count starts from 1.\n"
    ++ "If the [profile #] is abscent or is 0 then \n"
    ++ "the default configuration will be used:\n"
    ++ show [defSettings]
  exitWith $ ExitFailure 1

