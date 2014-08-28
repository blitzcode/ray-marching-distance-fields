
{-# LANGUAGE RecordWildCards #-}

module FileModChecker ( mkFileModChecker
                      , FileMod
                      , isModified
                      ) where

import Data.Time
import System.Directory

import Timing

-- Check for modifications in a file by comparing its time stamp at regular intervals

data FileMod = FileMod { fmFileName      :: !FilePath
                       , fmLastTimeStamp :: !UTCTime
                       , fmLastCheck     :: !Double
                       , fmCheckInterval :: !Double
                       }

mkFileModChecker :: FilePath -> Double -> IO FileMod
mkFileModChecker fmFileName fmCheckInterval = do
    fmLastCheck     <- getTick
    fmLastTimeStamp <- getModificationTime fmFileName
    return $ FileMod { .. }

isModified :: FileMod -> IO (FileMod, Bool)
isModified  fm@FileMod { .. } = do
    tick <- getTick
    if tick - fmLastCheck < fmCheckInterval
        then return (fm, False)
        else do ts <- getModificationTime fmFileName
                return (fm { fmLastCheck = tick, fmLastTimeStamp = ts } , ts /= fmLastTimeStamp )

