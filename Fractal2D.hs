
module Fractal2D ( mandelbrot
                 ) where

import Control.Loop
import Data.Complex
import Data.Word
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM

magnitudeSq :: RealFloat a => Complex a -> a
magnitudeSq c = realPart c * realPart c + imagPart c * imagPart c

-- http://linas.org/art-gallery/escape/escape.html
-- http://en.wikipedia.org/wiki/Mandelbrot_set#Continuous_.28smooth.29_coloring
fractionalIterCnt :: Int -> Complex Float -> Float
fractionalIterCnt iter escZ = fromIntegral iter - (log (log $ magnitudeSq escZ)) / log 2

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

-- http://en.wikipedia.org/wiki/Julia_set
-- http://www.relativitybook.com/CoolStuff/julia_set.html
{-
    let scaledTick = snd . properFraction $ tick / 17
        scaledTick2 = snd . properFraction $ tick / 61
        scaledTick3 = snd . properFraction $ tick / 71
        twoPi  = scaledTick * 2 * pi
        juliaR = sin twoPi * max 0.7 scaledTick2
        juliaI = cos twoPi * max 0.7 scaledTick3
-}

