{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  System.Process.Vado
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

module System.Process.Vado (
    MountPoint(..)
  , parseMountPoint
  , getMountPoint
  , MountSettings(..)
  , readSettings
  , defMountSettings
  , vado
  , vamount
) where

import Prelude hiding (null)
import Control.Applicative ((<$>), (<*), (*>), (<|>))
import Data.Text (pack, unpack, Text, null)
import Data.List (isPrefixOf, find)
import Data.Monoid (mconcat, mappend)
import Data.Attoparsec.Text (parseOnly, string, Parser, IResult(..), option)
import qualified Data.Attoparsec.Text as P (takeWhile1)
import Data.Text.IO (hPutStrLn)
import System.FilePath (addTrailingPathSeparator, makeRelative, (</>))
import Data.Maybe (mapMaybe, fromMaybe)
#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#else
import Text.Read (reads)
#endif
import System.Exit (ExitCode)
import System.Process (readProcess)
import System.Directory (getHomeDirectory, getCurrentDirectory, doesFileExist)

#if !MIN_VERSION_base(4,6,0)
-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
#endif

-- | Remote file system mount point
data MountPoint = MountPoint {
    remoteUser :: Text     -- ^ Account used on remote machine
  , remoteHost :: Text     -- ^ Host name or address of the remote machine
  , remoteDir  :: FilePath -- ^ Directory on remote machine
  , localDir   :: FilePath -- ^ Where it is mounted on this machine
  } deriving (Ord, Eq)

instance Show MountPoint where
    show MountPoint {..} = unpack (mconcat [remoteUser, "@", remoteHost, ":"])
                            ++ remoteDir ++ " on " ++ localDir ++ " "

-- | Mount point settings
data MountSettings = MountSettings {
    sshfsUser :: Text
  , sshfsHost :: Text
  , sshfsPort :: Int
  , idFile     :: FilePath
  } deriving (Show, Read)

-- | Default mount settings for vagrant
defMountSettings :: IO MountSettings
defMountSettings = do
  homeDir <- getHomeDirectory
  return MountSettings {
    sshfsUser = "vagrant"
  , sshfsHost = "127.0.0.1"
  , sshfsPort = 2222
  , idFile = homeDir </> ".vagrant.d/insecure_private_key"
  }


-- | Parser for a line of output from the 'mount' command
mountPointParser :: Parser MountPoint
mountPointParser = do
    remoteUser <- option "" (P.takeWhile1 (/= '@') <* string "@")
    remoteHost <- (string "[" *> P.takeWhile1 (/= ']') <* string "]") <|> P.takeWhile1 (/= ':')
    string ":"
    remoteDir <- unpack <$> P.takeWhile1 (/= ' ')
    string " on "
    localDir <- unpack <$> P.takeWhile1 (/= ' ')
    return MountPoint{..}

-- | Parses a line looking for a remote mount point
parseMountPoint :: String           -- ^ line of output fromt he 'mount' command
                -> Maybe MountPoint
parseMountPoint = done . parseOnly mountPointParser . pack
  where
    done (Right x)  = Just x
    done _          = Nothing

-- | Run 'mount' and look up the mount point relating to the
-- directory in the output
getMountPoint :: FilePath                      -- ^ Local directory to find the mount point
              -> IO (Either MountPoint String) -- ^ Details of the mount point or an error string
getMountPoint dir = do
    let dir' = addTrailingPathSeparator dir
    -- Run 'mount' and find the remote mount points
    mountPoints <- mapMaybe parseMountPoint . lines <$>
                    readProcess "mount" [] ""
    -- Find mount point that matches the current directory
    case filter ((`isPrefixOf` dir')
                . addTrailingPathSeparator
                . localDir) mountPoints of
        [mp] -> return $ Left mp
        _    -> return . Right $ "Mount point not found for the current directory ("
                        ++ dir ++ ")\n\n"
                        ++ case mountPoints of
                            [] -> "No remote mount points found in output of 'mount'"
                            _  -> "The following remote mount points were not suitable\n"
                                    ++ concatMap (\mp -> "  " ++ show mp ++ "\n") mountPoints


-- | Read a list of predefined mount points from the
-- ~/.vadosettings files
readSettings :: IO [MountSettings]
readSettings = do
  homeDir <- getHomeDirectory
  settings :: Maybe [MountSettings] <- do
      let settingsFile = homeDir </> ".vadosettings"
      exists <- doesFileExist settingsFile
      if exists
        then readMaybe <$> readFile settingsFile
        else return Nothing
  defaultSettings <- defMountSettings
  return $ fromMaybe [defaultSettings] settings

-- | Get a list of arguments to pass to ssh to run command on a remote machine
--   in the directory that is mounted locally
vado :: MountPoint      -- ^ Mount point found using 'getMountPoint'
     -> [MountSettings] -- ^ SSH settings from the '.vadosettings' files
     -> FilePath        -- ^ Local directory you want the command to run in.
                        --   Normally this will be the same directory
                        --   you passed to 'getMountPoint'.
                        --   The vado will run the command in the remote
                        --   directory that maps to this one.
     -> [String]        -- ^ Options to pass to ssh. If the mount point is 'vagrant@127.0.0.1'
                        --   then the most common vagrant connection options
                        --   ('-p2222' and '-i~/.vagrant.d/insecure_private_key')
                        --   are included automatically
     -> FilePath        -- ^ Command to run
     -> [String]       -- ^ Arguments to pass to the command
     -> IO [String]    -- ^ Full list of arguments that should be passed to ssh
vado MountPoint{..} settings cwd sshopts cmd args = do
    homeDir <- getHomeDirectory
    -- Work out where the current directory is on the remote machine
    let destinationDir = remoteDir </> makeRelative localDir cwd
    -- Run ssh with
    return $
        [unpack $ (if null remoteUser then "" else remoteUser `mappend` "@")
                  `mappend` remoteHost]
        ++ case find (\MountSettings{..} ->
                       remoteUser == sshfsUser
                       && remoteHost == sshfsHost) settings of
             Just MountSettings{..} ->
               [ "-p" ++ show sshfsPort
               , "-i" ++ idFile ]
             Nothing -> []
        ++ sshopts
        ++ ["cd", translate destinationDir, "&&", cmd]
        ++ args
  where
    translate str = '\'' : foldr escape "'" str
      where escape '\'' = showString "'\\''"
            escape c    = showChar c

-- | Get a list of arguments to pass to sshfs to
--   mount a remote filesystem in the given directory
vamount :: MountSettings -- ^ Mount settings to use
        -> FilePath      -- ^ Remote directory to mount
        -> FilePath      -- ^ Local directory (where to mount)
        -> [String]      -- ^ Other options to pass to sshfs
        -> [String]      -- ^ Resulting list of arguments
vamount MountSettings{..} remoteDir localDir opts =
  [ unpack $ mconcat
    [ sshfsUser, "@", sshfsHost
    , ":", pack remoteDir]
  , localDir
  , "-p" ++ show sshfsPort
  , "-oIdentityFile=" ++ idFile ] ++ opts
