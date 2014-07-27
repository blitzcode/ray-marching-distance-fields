
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import GHC.Conc (getNumProcessors)
import Control.Applicative
import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.STM.TQueue
import Data.List
import qualified System.Info as SI

import App
import Trace
import GLFWHelpers
import GLHelpers
import Timing
import Font
import qualified BoundedSequence as BS

runOnAllCores :: IO ()
runOnAllCores = GHC.Conc.getNumProcessors >>= setNumCapabilities

traceSystemInfo :: IO ()
traceSystemInfo = do
    cpus <- GHC.Conc.getNumProcessors
    traceS TLInfo =<<
        ( (++) . concat . intersperse " · " $
             [ "System - OS: " ++ SI.os
             , "Arch: " ++ SI.arch
             , "CPUs: " ++ show cpus
             , concat [ "Compiler: "
                      , SI.compilerName
                      , " / "
                      , show SI.compilerVersion
                      ]
             ]
        )
        <$> getGLStrings

main :: IO ()
main = do
    runOnAllCores
    withTrace Nothing True False True TLInfo $ do
      _aeGLFWEventsQueue <- newTQueueIO :: IO (TQueue GLFWEvent)
      withWindow 640 320 "Fractal" _aeGLFWEventsQueue $ \_aeWindow -> do
        traceSystemInfo
        withFontTexture $ \_aeFontTexture -> do
          _asCurTick <- getTick
          let as = AppState { _asLastEscPress = -1
                            , _asFrameTimes = BS.empty 250 -- Average over last 250 FPS
                            , ..
                            }
              ae = AppEnv { .. }
           in runAppT as ae run

