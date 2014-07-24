
module Timing ( getTick
              , timeIt
              ) where

import Data.Time.Clock
import Control.Applicative
import Control.Monad.IO.Class
import System.IO.Unsafe

-- Timing functions

-- TODO: Consider just using the criterion package for all performance measurements
--       http://hackage.haskell.org/package/criterion

{-# NOINLINE startTime #-}
startTime :: UTCTime
startTime = unsafePerformIO getCurrentTime

-- In seconds
getTick :: IO Double
getTick =
    -- TODO: Compare with GLFW timer
    --
    -- Just time <- GLFW.getTime
    -- return time
    realToFrac <$> flip diffUTCTime startTime <$> getCurrentTime

timeIt :: MonadIO m => m a -> m (Double, a)
timeIt f = do
    start <- liftIO getCurrentTime
    r <- f
    end <- liftIO getCurrentTime
    return (realToFrac $ diffUTCTime end start, r)

