
{-# LANGUAGE LambdaCase #-}

module HDREnvMap ( loadHDRImage
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
--import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Codec.Picture as JP
import Linear

import GLHelpers

loadHDRImage :: FilePath -> IO (Either String (JP.Image JP.PixelRGBF))
loadHDRImage fn =
    liftIO $ JP.readImage fn >>= \case
        Right (JP.ImageRGBF img) -> return $ Right img
        Left err                 -> return $ Left err
        _                        -> return . Left  $ "Not an HDR RGBF image: " ++ fn

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
        -- setTextureClampST GL.TextureCubeMap

        forM_ [ GL.TextureCubeMapPositiveX	
              , GL.TextureCubeMapNegativeX	
              , GL.TextureCubeMapPositiveY	
              , GL.TextureCubeMapNegativeY	
              , GL.TextureCubeMapPositiveZ	
              , GL.TextureCubeMapNegativeZ	
              ] $ \face -> do
          faceImg <- VSM.new $ w * w * 4 :: IO (VSM.IOVector Float)
          forM_ [0..w - 1] $ \y -> forM_ [0..w - 1] $ \x -> do
              let idx              = x * 4 + y * w * 4
                  writeChannel c v = VSM.write faceImg (idx + c) v
                  dir              = cubeMapPixelToDir face size x y
              writeChannel 0 $ dir ^. _x
              writeChannel 1 $ dir ^. _y
              writeChannel 2 $ dir ^. _z
              writeChannel 3 1
          VSM.unsafeWith faceImg $
              GL.texImage2D face GL.NoProxy 0 GL.RGBA32F size 0 . GL.PixelData GL.RGBA GL.Float
        traceOnGLError $ Just "latLongHDREnvMapToCubeMap"
        return tex

