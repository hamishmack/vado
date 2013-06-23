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
import System.Process.Vado (getMountPoint, vado)

-- | Main function for vado
main = do
    args <- getArgs
    case span ("-" `isPrefixOf`) args of
        (sshopts,cmd:rest) -> do
            currentDir <- getCurrentDirectory
            mbMountPoint <- getMountPoint currentDir
            case mbMountPoint of
                Left mp   -> vado mp currentDir sshopts cmd rest
                               >>= rawSystem "ssh" >>= exitWith
                Right err -> hPutStrLn stderr err >> (exitWith $ ExitFailure 1)
        _ -> do
            hPutStrLn stderr $
                "Usage vado [ssh options] command [args]\n\n"

                ++ "The command will be run in the directoy on the remote\n"
                ++ "machine that corrisponds to the current directory locally.\n\n"

                ++ "The ssh options must start with a dash '-'.\n"
                ++ "If the mount point is 'vagrant@127.0.0.1'\n"
                ++ "then the most common vagrant connection options\n"
                ++ "  -p2222 and -i~/.vagrant.d/insecure_private_key\n"
                ++ "are included automatically."
            exitWith $ ExitFailure 1

