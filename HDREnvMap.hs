
{-# LANGUAGE LambdaCase, BangPatterns #-}

module HDREnvMap ( loadHDRImage
                 , buildTestLatLongEnvMap
                 , cubeMapPixelToDir
                 , latLongHDREnvMapToCubeMap
                 , resizeHDRImage
                 , cosineConvolveHDREnvMap
                 ) where

import Control.Monad
import Control.Exception
import Control.Lens
import Control.Loop
import Data.List
import Text.Printf
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JPT
import Linear

import GLHelpers
import CoordTransf
import Trace
import ConcurrentSegments

loadHDRImage :: FilePath -> IO (Either String (JP.Image JP.PixelRGBF))
loadHDRImage fn = do
    JP.readImage fn >>= \case
        Right (JP.ImageRGBF img) -> do
            -- Trace intensity bounds of the image
            when (False) $
                let (minIntensity, maxIntensity, avgIntensity) = JPT.pixelFold
                      (\(!minI, !maxI, !avgI) _ _ (JP.PixelRGBF r g b) ->
                           let int = (r + g + b) / 3
                            in (min minI int, max maxI int, avgI + int)
                      )
                      (10000000, 0, 0)
                      img
                 in traceS TLInfo $ printf
                        "Loaded HDR image '%s', min int: %f, max int: %f, avg int: %f"
                        fn
                        minIntensity
                        maxIntensity
                        (avgIntensity / (fromIntegral $ JP.imageWidth img * JP.imageHeight img))
            return $ Right img
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
             -- Centering like this ensures clean filtering across cube map seams
    let vw = (fromIntegral x + 0.5) / fromIntegral w * 2 - 1
        vh = (fromIntegral y + 0.5) / fromIntegral h * 2 - 1
     in normalize $ case face of
            GL.TextureCubeMapPositiveX -> V3    1  (-vh) (-vw)
            GL.TextureCubeMapNegativeX -> V3  (-1) (-vh)   vw
            GL.TextureCubeMapPositiveY -> V3   vw     1    vh
            GL.TextureCubeMapNegativeY -> V3   vw   (-1) (-vh)
            GL.TextureCubeMapPositiveZ -> V3   vw  (-vh)    1
            GL.TextureCubeMapNegativeZ -> V3 (-vw) (-vh)  (-1)

-- Bilinear lookup into an HDR environment map
-- http://en.wikipedia.org/wiki/Bilinear_filtering#Sample_code
pixelAtBilinear :: JP.Image JP.PixelRGBF -> Float -> Float -> V3 Float
pixelAtBilinear img u v =
    let w         = JP.imageWidth  img
        h         = JP.imageHeight img
        -- Texel center at (0, 0)
        upx       = u * (fromIntegral w - 1)
        upy       = v * (fromIntegral h - 1)
        x         = floor upx
        y         = floor upy
        xp1       = (x + 1) `mod` (w - 1)
        -- TODO: Fix this properly. We can wrap around on the X axis, but for Y we can't wrap
        --       from top to bottom, but would instead need to reflect X around the center
        --       of the axis to get the correct texel
        yp1       = min (h - 1) (y + 1)
        uRatio    = upx - fromIntegral x
        vRatio    = upy - fromIntegral y
        uOpposite = 1 - uRatio
        vOpposite = 1 - vRatio
        tex xc yc = case JP.unsafePixelAt (JP.imageData img) (xc * 3 + yc * w * 3) of
                        (JP.PixelRGBF r g b) -> V3 r g b
        -- tex xc yc = case JP.pixelAt img xc yc of (JP.PixelRGBF r g b) -> V3 r g b
     in (tex x y   ^* uOpposite + tex xp1 y   ^* uRatio) ^* vOpposite +
        (tex x yp1 ^* uOpposite + tex xp1 yp1 ^* uRatio) ^* vRatio

-- Transform a latitude / longitude format environment map into a cube map texture. This
-- creates some distortion and we only use basic bilinear lookups (no full texel coverage)
-- for the resampling, introducing artifacts in the process. Results look fairly good, though
latLongHDREnvMapToCubeMap :: JP.Image JP.PixelRGBF -> Bool -> IO GL.TextureObject
latLongHDREnvMapToCubeMap latlong debugFaceColorize =
    bracketOnError GL.genObjectName GL.deleteObjectName $ \tex -> do
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
            faceImg <- VSM.new $ w * w :: IO (VSM.IOVector (V3 Float))
            void $ forSegmentsConcurrently Nothing 0 w $ \start end -> -- In parallel
                forM_ [start..end - 1] $ \y -> forM_ [0..w - 1] $ \x ->
                    let idx          = x + y * w
                        -- Convert from a cube map texel to a lat./long. environment map texel
                        dir          = cubeMapPixelToDir face size x y
                        (theta, phi) = cartesianToSpherical $ worldToLocal dir
                        (u, v)       = sphericalToEnvironmentUV theta phi
                        -- Lookup source texel
                        col          = pixelAtBilinear latlong u v
                        -- We can colorize the faces of the cube for debugging purposes
                        colFace      = case face of
                                           GL.TextureCubeMapPositiveX -> V3 1 0 0 -- Red
                                           GL.TextureCubeMapNegativeX -> V3 0 1 0 -- Green
                                           GL.TextureCubeMapPositiveY -> V3 0 0 1 -- Blue
                                           GL.TextureCubeMapNegativeY -> V3 1 0 1 -- Pink
                                           GL.TextureCubeMapPositiveZ -> V3 1 1 0 -- Yellow
                                           GL.TextureCubeMapNegativeZ -> V3 0 1 1 -- Cyan
                     in VSM.write faceImg idx $ col * if debugFaceColorize then colFace else 1
                        -- Debug output normal
                        -- VSM.write faceImg idx $ V3 (dir ^. _x) (dir ^. _y) (dir ^. _z)
            -- Upload and let OpenGL convert to half floats
            VSM.unsafeWith faceImg $
                GL.texImage2D face GL.NoProxy 0 GL.RGB16F size 0 . GL.PixelData GL.RGB GL.Float
        traceOnGLError $ Just "latLongHDREnvMapToCubeMap"
        return tex

-- Scale an HDR image to a given target width (keep aspect for height). This is certainly
-- not the most sophisticated way to do image scaling and will produce poor results for
-- small changes in size and upscaling in general. For our actual use case (downscaling an
-- HDR environment map prior to convolution) its quality is absolutely adequate
resizeHDRImage :: JP.Image JP.PixelRGBF -> Int -> JP.Image JP.PixelRGBF
resizeHDRImage src dstw =
  let srcw  = JP.imageWidth  src
      srch  = JP.imageHeight src
      dsth  = round $ (fromIntegral srch / fromIntegral srcw * fromIntegral dstw :: Float)
      scale = fromIntegral srcw / fromIntegral dstw :: Float
      taps  = ceiling $ scale :: Int
      ntaps = fromIntegral $ taps * taps
      step  = scale / fromIntegral taps
      scaled dstx dsty =
        let srcx1 = fromIntegral dstx * scale
            srcy1 = fromIntegral dsty * scale
         in (\(JP.PixelRGBF r g b) -> JP.PixelRGBF (r / ntaps) (g / ntaps) (b / ntaps)) $ foldl'
              (\(JP.PixelRGBF ar ag ab) (srcx, srcy) ->
                 let u        = srcx / (fromIntegral srcw - 1)
                     v        = srcy / (fromIntegral srch - 1)
                     V3 r g b = pixelAtBilinear src u v
                  in JP.PixelRGBF (ar + r) (ag + g) (ab + b)
              )
              (JP.PixelRGBF 0 0 0)
              [ ( srcx1 + fromIntegral x * step
                , srcy1 + fromIntegral y * step
                )
                | y <- [0..taps - 1]
                , x <- [0..taps - 1]
              ]
   in JP.generateImage scaled dstw dsth

-- TODO: There's plenty of room for improvement regarding our handling of pre-convolved
--       environment maps. We could do the convolution in frequency space with SH,
--       possibly even at runtime to save memory and allow for multiple exponents, see here:
--
--       http://www.cs.columbia.edu/~cs4162/slides/spherical-harmonic-lighting.pdf
--       http://www.ppsloan.org/publications/StupidSH36.pdf
--
--       We could also adopt improvements from AMD's cubemapgen tool, as described here:
--
--       http://seblagarde.wordpress.com/2012/06/10/amd-cubemapgen-for-physically-based-rendering/
--       https://code.google.com/p/cubemapgen/

-- Convolve an environment map with a cosine lobe. This is a rather slow O(n^4)
-- operation. A resolution of 256x128 is both sufficient and probably the most
-- that is computationally feasible
--
-- TODO: We add a lot of values together that might be either very small or very large.
--       Analyze if the 32 bit intermediate floats, the 32 bit RGBE values in the HDR
--       file and the RGB16F on the GPU are always sufficient
--
cosineConvolveHDREnvMap :: JP.Image JP.PixelRGBF -> Float -> JP.Image JP.PixelRGBF
cosineConvolveHDREnvMap src power =
  let srcw        = JP.imageWidth  src
      srch        = JP.imageHeight src
      -- We don't care about the actual correct angles as stored in the environment map,
      -- only their difference matters
      pxToTheta p = fromIntegral p / fromIntegral (srch - 1) * pi
      pxToPhi   p = fromIntegral p / fromIntegral (srcw - 1) * 2 * pi
      convolve dstx dsty =
        let thetaLobe           = pxToTheta dsty
            thetaLobeCos        = cos thetaLobe
            thetaLobeSin        = sin thetaLobe
            phiLobe             = pxToPhi dstx
            absPhiDiffCosLookup = VU.generate srcw (\x -> cos . abs $ phiLobe - pxToPhi x)
            -- Divide sum by number of hemisphere samples
         in (\(r, g, b, n) -> JP.PixelRGBF (r / n) (g / n) (b / n)) $
              forLoopFold 0 (< srch) (+ 1) (0, 0, 0, 0) $ \accY y ->
                let thetaPx    = pxToTheta y
                    thetaPxCos = cos thetaPx
                    thetaPxSin = sin thetaPx
                 in forLoopFold 0 (< srcw) (+ 1) accY $ \(!ar, !ag, !ab, !n) x ->
                      let JP.PixelRGBF r g b = JP.unsafePixelAt (JP.imageData src)
                                                                (x * 3 + y * srcw * 3)
                          -- Basically a dot product in spherical coordinates
                          -- http://en.wikipedia.org/wiki/Great-circle_distance#Formulas
                          cosAngle = thetaLobeCos * thetaPxCos +
                                     thetaLobeSin * thetaPxSin *
                                     VU.unsafeIndex absPhiDiffCosLookup x
                          -- TODO: That power takes a good chunk of the overall runtime,
                          --       consider using a lookup table
                          cosAnglePow = cosAngle ** power
                          -- Sin theta factor to account for area distortion in the lat/long
                          -- parameterization of the sphere
                          fac = thetaPxSin * cosAnglePow
                       in if   cosAngle > 0
                          then (ar + (r * fac), ag + (g * fac), ab + (b * fac), n + 1)
                          else (ar, ag, ab, n)
   in JP.generateImage convolve srcw srch

