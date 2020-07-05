-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
-- Stability   :  Experimental
-- Portability :  Unknown
--
-- | Lets you quickly run ssh on a machine that you have an sshfs connection
-- to.  It works out the username, host and the directory on the host based
-- on the current directory and the output of 'mount'
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.IO (hPutStrLn, stderr)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import System.Exit (exitWith, ExitCode(..))
import System.Process (rawSystem)
import System.Process.Vado (getMountPoint, vado, readSettings, defMountSettings)

-- | Main function for vado
main = do
    args <- getArgs
    case span ("-" `isPrefixOf`) args of
        (sshopts,cmd:rest) -> do
            currentDir <- getCurrentDirectory
            mbMountPoint <- getMountPoint currentDir
            ms <- readSettings
            case mbMountPoint of
                Left mp   -> vado mp ms currentDir sshopts cmd rest
                               >>= rawSystem "ssh" >>= exitWith
                Right err -> hPutStrLn stderr err >> (exitWith $ ExitFailure 1)
        _ -> do
            defSettings <- defMountSettings
            hPutStrLn stderr $
                "Usage vado [ssh options] command [args]\n\n"

                ++ "The command will be run in the directoy on the remote\n"
                ++ "machine that corrisponds to the current directory locally.\n\n"

                ++ "The ssh options must start with a dash '-'.\n"
                ++ "You can specify port and key location settings\n"
                ++ "in the ~/.vadosettings file.\nExample contents:\n"
                ++ show [defSettings]
            exitWith $ ExitFailure 1

