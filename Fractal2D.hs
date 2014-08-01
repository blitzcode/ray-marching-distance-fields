
{-# LANGUAGE BangPatterns #-}

module Fractal2D ( mandelbrot
                 , juliaAnimated
                 , makeSegments
                 ) where

import Control.Loop
import Control.Monad
import Control.Concurrent.Async
import Data.Complex
import Data.Word
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Concurrent (getNumCapabilities)

magnitudeSq :: RealFloat a => Complex a -> a
magnitudeSq c = realPart c * realPart c + imagPart c * imagPart c

-- http://linas.org/art-gallery/escape/escape.html
-- http://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_.28smooth.29_coloring
fractionalIterCnt :: Int -> Complex Float -> Float
fractionalIterCnt iter escZ = max 0 $ fromIntegral iter - (log (log $ magnitudeSq escZ)) / log 2

-- http://en.wikipedia.org/wiki/Mandelbrot_set#Computer_drawings
mandelbrot :: Int -> Int -> VSM.IOVector Word32 -> Bool -> IO ()
mandelbrot w h fb smooth =
  forLoop 0 (< h) (+ 1) $ \py -> forLoop 0 (< w) (+ 1) $ \px ->
    let idx          = px + py * w
        fpx          = fromIntegral px :: Float
        fpy          = fromIntegral py :: Float
        fw           = fromIntegral w  :: Float
        fh           = fromIntegral h  :: Float
        ratio        = fw / fh
        y            = (fpy / fh) * 2 - 1 -- Y axis is [-1, +1]
        xshift       = (- 2) - ((2 * ratio - 2.5) * 0.5)
        x            = (fpx / fw) * 2 * ratio + xshift -- Keep aspect and center [-1, +0.5]
        c            = x :+ y
        maxIter      = 40
        (iCnt, escZ) = go (0 :: Int) (0 :+ 0)
        go iter z | (iter == maxIter) || -- Iteration limit?
                    magnitudeSq z > 4 * 4 = (iter, z) -- Hit escape radius?
                  | otherwise = let newZ = z * z + c
                                 in if   newZ == z -- Simple 1-cycle detection
                                    then (maxIter, z)
                                    else go (iter + 1) newZ
        icCont | iCnt == maxIter = fromIntegral maxIter -- Interior in case of limit
               | otherwise       = fractionalIterCnt iCnt escZ
        toGreen v = v `shiftL` 8
     in VSM.unsafeWrite fb idx $
          if   smooth
          then toGreen $ truncate (icCont / fromIntegral maxIter * 255 :: Float)
          else toGreen $ truncate ((fromIntegral iCnt / fromIntegral maxIter) * 255 :: Float)

-- Split an interval into evenly spaced segments
makeSegments :: Int -> Int -> Int -> [(Int, Int)]
makeSegments nseg low high =
    map (\i -> (low + i * segl, low + (i + 1) * segl - 1)) [0..nseg - 2] ++ end
  where range = high - low
        segl  = (range `div` nseg)
        end   = [(low + (nseg - 1) * segl, high - 1)]

-- http://en.wikipedia.org/wiki/Julia_set
-- http://www.relativitybook.com/CoolStuff/julia_set.html
juliaAnimated :: Int -> Int -> VSM.IOVector Word32 -> Bool -> Double -> IO ()
juliaAnimated w h fb smooth tick = do
  nCPUs <- getNumCapabilities
  let !fTick         = realToFrac tick :: Float
      !scaledTick    = snd (properFraction $ fTick / 17 :: (Int, Float))
      !scaledTick2   = snd (properFraction $ fTick / 61 :: (Int, Float))
      !scaledTick3   = snd (properFraction $ fTick / 71 :: (Int, Float))
      !twoPi         = scaledTick * 2 * pi
      !juliaR        = sin twoPi * max 0.7 scaledTick2
      !juliaI        = cos twoPi * max 0.7 scaledTick3
      !fw            = fromIntegral w  :: Float
      !fh            = fromIntegral h  :: Float
      !ratio         = fw / fh
      !xshift        = 1.45 * ratio
      !maxIter       = 40
      !segments      = makeSegments nCPUs 0 h
      doSeg (lo, hi) = forLoop lo (<= hi) (+ 1) $ \py -> forLoop 0 (< w) (+ 1) $ \px ->
        let idx          = px + py * w
            fpx          = fromIntegral px :: Float
            fpy          = fromIntegral py :: Float
            y            = (fpy / fh) * 2.9 - 1.45 -- Y axis is [-1.45, +1.45]
            x            = (fpx / fw) * 2.9 * ratio - xshift -- Keep aspect and center
            c            = x :+ y
            (iCnt, escZ) = go (0 :: Int) c
            go iter z | (iter == maxIter) || -- Iteration limit?
                        magnitudeSq z > 4 * 4 = (iter, z) -- Hit escape radius?
                      | otherwise = let newZ = z * z + (juliaR :+ juliaI)
                                     in if   newZ == z -- Simple 1-cycle detection
                                        then (maxIter, z)
                                        else go (iter + 1) newZ
            icCont | iCnt == maxIter = fromIntegral maxIter -- Interior in case of limit
                   | otherwise       = fractionalIterCnt iCnt escZ
            toGreen v = v `unsafeShiftL` 8
         in VSM.unsafeWrite fb idx $
              if   smooth
              then toGreen $ truncate (icCont / fromIntegral maxIter * 255 :: Float)
              else toGreen $ truncate ((fromIntegral iCnt / fromIntegral maxIter) * 255 :: Float)
   in void $ mapConcurrently doSeg segments

