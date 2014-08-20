
module CoordTransf ( sphericalToCartesian
                   , cartesianToSpherical
                   , worldToLocal
                   , localToWorld
                   , sphericalToEnvironmentUV
                   , sphericalToEnvironmentPx
                   , environmentUVToSpherical
                   , environmentPxToSpherical
                   ) where

import Control.Exception
import Control.Lens
import Linear

-- Coordinate system transforms, Haskell port from my own code inside
-- http://www.blitzcode.net/3d_1.shtml#Product_Importance_Sampling
--
-- We use right handed systems everywhere, world space having Y as up, X going
-- right and Z out of the screen (OpenGL style). Surface local coordinates
-- have Z going up and X/Y as right / up on the surface. Spherical coordinates
-- have Theta as the polar angle [0, Pi] relative to the Z axis and Phi as the
-- azimuthal angle [0, 2Pi) in the X, Y plane relative to X axis. Environment
-- maps and other data stored in latitude / longitude format has down (-Y,
-- theta = Pi) at y = 0 and forward (-Z, phi = Pi/2) at x = width / 2.

sphericalToCartesian :: Float -> Float -> V3 Float
sphericalToCartesian theta phi =
    (theta >= 0 && theta <= pi) `assert`
    (phi >= 0 && phi <= 2 * pi) `assert`
    V3 (sin theta * cos phi)
       (sin theta * sin phi)
       (cos theta          )

cartesianToSpherical :: V3 Float -> (Float, Float)
cartesianToSpherical dir =
    (theta >= 0 && theta <= pi) `assert`
    (phi >= 0 && phi < 2 * pi)  `assert`
    (theta, phi)
  where theta         = acos $ clamp (dir ^. _z) (-1) 1
        phi''         = atan2 (dir ^. _y) (dir ^. _x)
        phi'          = if phi'' < 0 then phi'' + 2 * pi else phi''
        phi           = if phi' == 2 * pi then 0 else phi'
        clamp v mi ma = max mi $ min ma v

worldToLocal :: V3 Float -> V3 Float
worldToLocal world = V3 (dot world x) (dot world y) (dot world n)
    where n = V3 0  1   0
          x = V3 1  0   0
          y = V3 0  0 (-1)

localToWorld :: V3 Float -> V3 Float
localToWorld local = V3 (local^._x * x^._x + local^._y * y^._x + local^._z * n^._x)
                        (local^._x * x^._y + local^._y * y^._y + local^._z * n^._y)
                        (local^._x * x^._z + local^._y * y^._z + local^._z * n^._z)
    where n = V3 0  1   0
          x = V3 1  0   0
          y = V3 0  0 (-1)

sphericalToEnvironmentUV :: Float -> Float -> (Float, Float)
sphericalToEnvironmentUV theta phi = (u, v)
    where -- Forward is in the center of our HDR image. Our coordinate system
          -- conventions say Phi = 0 means it is aligned with the X-axis, pointing
          -- right. Add Pi / 2 which rotates 90 degrees CCW, we're aligned now
          phi'   = phi + pi / 2;
          phi''  = if phi' > 2 * pi then phi' - 2 * pi else phi'
          -- We assume our images are stored CW, Phi rotates CCW around Z. Invert
          phi''' = 2 * pi - phi''
          -- Theta = 0 should be straight up, but the way our HDR image is stored the
          -- first row of pixels is the bottom, invert
          theta' = pi - theta
          u      = phi''' / (pi * 2)
          v      = theta' / pi

sphericalToEnvironmentPx :: Float -> Float -> Int -> (Int, Int)
sphericalToEnvironmentPx theta phi width = (x, y)
    where (u, v) = sphericalToEnvironmentUV theta phi
          height = width `div` 2
          x      = round $ u * fromIntegral width
          y'     = round $ v * fromIntegral height
          y      = if y' == height then y' - 1 else y'

environmentUVToSpherical :: Float -> Float -> (Float, Float)
environmentUVToSpherical u v = (theta, phi)
    where theta = pi - v * pi
          phi'' = u * pi * 2 + pi / 2
          phi'  = if phi'' >= pi * 2 then phi'' - pi * 2 else phi''
          phi   = 2 * pi - phi'

environmentPxToSpherical :: Int -> Int -> Int -> (Float, Float)
environmentPxToSpherical x y width = environmentUVToSpherical u v
    where height = width `div` 2
          u      = fromIntegral x / fromIntegral (width  - 1)
          v      = fromIntegral y / fromIntegral (height - 1)

