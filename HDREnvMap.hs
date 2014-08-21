
{-# LANGUAGE LambdaCase #-}

module HDREnvMap ( loadHDRImage
                 , buildTestLatLongEnvMap
                 , cubeMapPixelToDir
                 , latLongHDREnvMapToCubeMap
                 ) where

import Control.Monad
import Control.Exception
import Control.Lens
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Codec.Picture as JP
import Linear

import GLHelpers
import CoordTransf

loadHDRImage :: FilePath -> IO (Either String (JP.Image JP.PixelRGBF))
loadHDRImage fn = do
    JP.readImage fn >>= \case
        Right (JP.ImageRGBF img) -> return $ Right img
        Left err                 -> return $ Left err
        _                        -> return . Left  $ "Not an HDR RGBF image: " ++ fn

-- Create an environment map representing a distant cube with colored faces
buildTestLatLongEnvMap :: JP.Image JP.PixelRGBF
buildTestLatLongEnvMap = JP.generateImage f w h
  where w        = 512
        h        = 256
        colRight = JP.PixelRGBF 1 0 0 -- Red
        colLeft  = JP.PixelRGBF 0 1 0 -- Green
        colUp    = JP.PixelRGBF 0 0 1 -- Blue
        colDown  = JP.PixelRGBF 1 0 1 -- Pink
        colFront = JP.PixelRGBF 1 1 0 -- Yellow
        colBack  = JP.PixelRGBF 0 1 1 -- Cyan
        f x y    = let (theta, phi) = environmentPxToSpherical x y w
                       dir          = localToWorld $ sphericalToCartesian theta phi
                    in case () of
                        _ | abs (dir^._x) >= abs (dir^._y) && abs (dir^._x) >= abs (dir^._z) ->
                                if dir^._x > 0 then colRight else colLeft
                          | abs (dir^._y) >= abs (dir^._x) && abs (dir^._y) >= abs (dir^._z) ->
                                if dir^._y > 0 then colUp    else colDown
                          | otherwise ->
                                if dir^._z < 0 then colFront else colBack

-- Get directional vector for a pixel on a cube map face
cubeMapPixelToDir :: GL.TextureTargetCubeMapFace -> GL.TextureSize2D -> Int -> Int -> V3 Float
cubeMapPixelToDir face (GL.TextureSize2D w h) x y =
    let vw               = (fromIntegral x + 0.5) / fromIntegral w * 2 - 1
        vh               = (fromIntegral y + 0.5) / fromIntegral h * 2 - 1
     in normalize $ case face of
            GL.TextureCubeMapPositiveX -> V3    1  (-vh) (-vw)
            GL.TextureCubeMapNegativeX -> V3  (-1) (-vh)   vw
            GL.TextureCubeMapPositiveY -> V3   vw     1    vh
            GL.TextureCubeMapNegativeY -> V3   vw   (-1) (-vh)
            GL.TextureCubeMapPositiveZ -> V3   vw  (-vh)    1
            GL.TextureCubeMapNegativeZ -> V3 (-vw) (-vh)  (-1)

-- http://en.wikipedia.org/wiki/Bilinear_filtering#Sample_code
pixelAtBilinear :: JP.Image JP.PixelRGBF -> Float -> Float -> V3 Float
pixelAtBilinear img u v =
    let w         = JP.imageWidth  img
        h         = JP.imageHeight img
        -- TODO: Take into account texel centers?
        upx       = u * (fromIntegral w)
        upy       = v * (fromIntegral h)
        x         = floor upx
        y         = floor upy
        xp1       = (x + 1) `mod` (w - 1)
        -- TODO: This is wrong. We can wrap around on the X axis, but for Y we can't wrap
        --       from top to bottom, but would instead need to reflect X around the center
        --       of the axis to get the correct texel
        yp1       = (y + 1) `mod` (h - 1)
        uRatio    = upx - fromIntegral x
        vRatio    = upy - fromIntegral y
        uOpposite = 1 - uRatio
        vOpposite = 1 - vRatio
        tex xc yc = case JP.pixelAt img xc yc of (JP.PixelRGBF r g b) -> V3 r g b
     in (tex x y   ^* uOpposite + tex xp1 y   ^* uRatio) ^* vOpposite +
        (tex x yp1 ^* uOpposite + tex xp1 yp1 ^* uRatio) ^* vRatio

-- Transform a latitude / longitude format environment map into a cube map texture. This
-- creates some distortion and we only use basic bilinear lookups (no full texel coverage)
-- to do the resampling, introducing artifacts in the process
latLongHDREnvMapToCubeMap :: JP.Image JP.PixelRGBF -> Bool -> IO GL.TextureObject
latLongHDREnvMapToCubeMap latlong debugFaceColorize =
  bracketOnError
    GL.genObjectName
    GL.deleteObjectName
    $ \tex -> do
        -- Setup cube map
        GL.textureBinding GL.TextureCubeMap GL.$= Just tex
        setTextureFiltering GL.TextureCubeMap TFMagOnly
        -- Apparently some older GPUs / drivers have issues with this, a simple
        -- 'setTextureClampST GL.TextureCubeMap' might also be sufficient
        GLR.glEnable GLR.gl_TEXTURE_CUBE_MAP_SEAMLESS
        -- Fill all six cube map faces
        let w    = JP.imageWidth latlong `div` 3 -- Three is a slight increase in texels,
                                                 -- four is a slight reduction
            size = GL.TextureSize2D (fromIntegral w) (fromIntegral w)
        forM_ [ GL.TextureCubeMapPositiveX
              , GL.TextureCubeMapNegativeX
              , GL.TextureCubeMapPositiveY
              , GL.TextureCubeMapNegativeY
              , GL.TextureCubeMapPositiveZ
              , GL.TextureCubeMapNegativeZ
              ] $ \face -> do
          faceImg <- VSM.new $ w * w :: IO (VSM.IOVector (V4 Float))
          forM_ [0..w - 1] $ \y -> forM_ [0..w - 1] $ \x ->
              let idx          = x + y * w
                  -- Convert from a cube map texel to a lat./long. environment map texel
                  dir          = cubeMapPixelToDir face size x y
                  (theta, phi) = cartesianToSpherical $ worldToLocal dir
                  (u, v)       = sphericalToEnvironmentUV theta phi
                  -- Lookup source texel
                  col          = pixelAtBilinear latlong u v
                  -- We can colorize the faces of the cube for debugging purposes
                  colFace      = case face of GL.TextureCubeMapPositiveX -> V4 1 0 0 1 -- Red
                                              GL.TextureCubeMapNegativeX -> V4 0 1 0 1 -- Green
                                              GL.TextureCubeMapPositiveY -> V4 0 0 1 1 -- Blue
                                              GL.TextureCubeMapNegativeY -> V4 1 0 1 1 -- Pink
                                              GL.TextureCubeMapPositiveZ -> V4 1 1 0 1 -- Yellow
                                              GL.TextureCubeMapNegativeZ -> V4 0 1 1 1 -- Cyan
               in VSM.write faceImg idx $ V4 (col ^. _x) (col ^. _y) (col ^. _z) 1 *
                      if debugFaceColorize then colFace else 1
                  -- Debug output normal
                  -- VSM.write faceImg idx $ V4 (dir ^. _x) (dir ^. _y) (dir ^. _z) 1
          -- Upload (TODO: Might want to use half floats and drop the alpha)
          VSM.unsafeWith faceImg $
              GL.texImage2D face GL.NoProxy 0 GL.RGBA32F size 0 . GL.PixelData GL.RGBA GL.Float
        traceOnGLError $ Just "latLongHDREnvMapToCubeMap"
        return tex

