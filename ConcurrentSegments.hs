
module ConcurrentSegments ( makeNSegments
                          , forSegmentsConcurrently
                          ) where

import Data.Maybe
import Control.Applicative
import Control.Concurrent.Async
import GHC.Conc (getNumProcessors)

-- Module for splitting an interval into even segments and processing them concurrently.
-- Typical use case would be a 1D or 2D array that is supposed to be processed on all
-- available cores

makeNSegments :: Int -> Int -> Int -> [(Int, Int)]
makeNSegments nseg low high
    | low >= high = []
    | nsegc <= 0  = []
    | nsegc == 1  = [(low, high)]
    | otherwise   = map (\i -> (low + i * segl, low + (i + 1) * segl)) [0..nsegc - 2] ++ end
  where range = high - low
        segl  = range `div` nsegc
        end   = [(low + (nsegc - 1) * segl, high)]
        nsegc = min nseg $ high - low

forSegmentsConcurrently :: Maybe Int -> Int -> Int -> (Int -> Int -> IO a) -> IO [a]
forSegmentsConcurrently mbNSeg low high f = do
    nseg <- fromMaybe getNumProcessors $ return <$> mbNSeg
    mapConcurrently (uncurry f) $ makeNSegments nseg low high

