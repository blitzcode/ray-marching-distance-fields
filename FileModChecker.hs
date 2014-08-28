
{-# LANGUAGE RecordWildCards #-}

module FileModChecker ( mkFileModChecker
                      , FileMod
                      , isModified
                      , checkModifedAsync
                      ) where

import Data.Time
import System.Directory
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

import Timing

-- Check for modifications in a file by comparing its time stamp at regular intervals.
-- Usage is either call mkFileModChecker, poll with isModified and maintain the FileMod
-- state manually or launch a worker thread with checkModifedAsync and poll using the
-- supplied function

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
                return ( fm { fmLastCheck = tick, fmLastTimeStamp = ts }
                       , ts /= fmLastTimeStamp
                       )

checkModifedAsync :: FilePath -> Double -> IO (IO Bool)
checkModifedAsync fn interval = do
    mv     <- newMVar False
    tsInit <- getModificationTime fn
    void . async $
        let loop ts = do threadDelay . round $ interval * 1000 * 1000
                         ts' <- getModificationTime fn
                         when (ts /= ts') $ modifyMVar_ mv (\_ -> return True)
                         loop ts'
         in loop tsInit
    return $ modifyMVar mv (\isMod -> return (False, isMod))

