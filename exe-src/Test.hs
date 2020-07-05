{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main (for tests)
-- Copyright   :  Hamish Mackenzie
-- License     :  BSD
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
-- Stability   :  Experimental
-- Portability :  Unknown
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Control.Applicative ((<$>))
import System.Exit (exitFailure)
import Test.QuickCheck (Arbitrary(..), elements, listOf1)
import Test.QuickCheck.All (quickCheckAll)
import Data.Text (pack)
import System.Process.Vado (MountPoint(..), parseMountPoint)

instance Arbitrary MountPoint where
    arbitrary = do
        let nameChars = ['A'..'Z'] ++ ['a' .. 'z'] ++ ['0'..'1'] ++ "_."
        remoteUser <- pack <$> listOf1 (elements nameChars)
        remoteHost <- pack <$> listOf1 (elements nameChars)
        remoteDir <- listOf1 (elements $ '/':nameChars)
        localDir <- listOf1 (elements $ '/':nameChars)
        return MountPoint{..}

prop_MountPointParses mp = parseMountPoint (show mp) == Just mp

-- | Entry point for unit tests.
main = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

