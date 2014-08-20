
{-# LANGUAGE LambdaCase #-}

module HDREnvMap ( loadHDRImage
                 , buildTestLatLongEnvMap
                 , cubeMapPixelToDir
                 , latLongHDREnvMapToCubeMap
                 ) where

--import Control.Applicative
import Control.Monad
import Control.Monad.Except
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
    let vw               = fromIntegral x / fromIntegral w * 2 - 1
        vh               = fromIntegral y / fromIntegral h * 2 - 1
     in normalize $ case face of
            GL.TextureCubeMapPositiveX -> V3    1  (-vh) (-vw)
            GL.TextureCubeMapNegativeX -> V3  (-1) (-vh)   vw
            GL.TextureCubeMapPositiveY -> V3   vw     1    vh
            GL.TextureCubeMapNegativeY -> V3   vw   (-1) (-vh)
            GL.TextureCubeMapPositiveZ -> V3   vw  (-vh)    1
            GL.TextureCubeMapNegativeZ -> V3 (-vw) (-vh)  (-1)

latLongHDREnvMapToCubeMap :: JP.Image JP.PixelRGBF -> IO GL.TextureObject
latLongHDREnvMapToCubeMap latlong =
  bracketOnError
    GL.genObjectName
    GL.deleteObjectName
    $ \tex -> do
        let w    = JP.imageWidth latlong `div` 4
            size = GL.TextureSize2D (fromIntegral w) (fromIntegral w)
        GL.textureBinding GL.TextureCubeMap GL.$= Just tex
        setTextureFiltering GL.TextureCubeMap TFMagOnly

        -- GLR.glEnable GLR.gl_TEXTURE_CUBE_MAP_SEAMLESS
        setTextureClampST GL.TextureCubeMap

        forM_ [ GL.TextureCubeMapPositiveX	
              , GL.TextureCubeMapNegativeX	
              , GL.TextureCubeMapPositiveY	
              , GL.TextureCubeMapNegativeY	
              , GL.TextureCubeMapPositiveZ	
              , GL.TextureCubeMapNegativeZ	
              ] $ \face -> do
          faceImg <- VSM.new $ w * w :: IO (VSM.IOVector (V4 Float))
          forM_ [0..w - 1] $ \y -> forM_ [0..w - 1] $ \x ->
              let idx = x + y * w
                  dir = cubeMapPixelToDir face size x y
               in VSM.write faceImg idx $ V4 (dir ^. _x) (dir ^. _y) (dir ^. _z) 1
          VSM.unsafeWith faceImg $
              GL.texImage2D face GL.NoProxy 0 GL.RGBA32F size 0 . GL.PixelData GL.RGBA GL.Float
        traceOnGLError $ Just "latLongHDREnvMapToCubeMap"
        return tex

