
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module GPUFractal3D ( withGPUFractal3D
                    , loadAndCompileShaders
                    , GPUFractal3D
                    , drawGPUFractal3D
                    , FractalShader(..)
                    ) where

import Control.Applicative
import Control.Exception
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Concurrent.Async
import System.Directory
import System.FilePath
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Codec.Picture as JP

import Trace
import Timing
import GLHelpers
import Shaders
import GPUFractal3DShaderSource
import HDREnvMap
import CornellBox

data GPUFractal3D = GPUFractal3D { gfVAO               :: !GL.VertexArrayObject
                                 , gfDECornellBoxShd   :: !GL.Program
                                 , gfDETestShd         :: !GL.Program
                                 , gfMBPower8Shd       :: !GL.Program
                                 , gfMBGeneralShd      :: !GL.Program
                                 , gfEnvCubeMaps       :: [(String, GL.TextureObject)]
                                 , gfCornellBoxGeomTex :: !GL.TextureObject
                                 }

data FractalShader = FSDECornellBoxShader | FSDETestShader | FSMBPower8Shader | FSMBGeneralShader
                     deriving (Show, Eq, Enum)

withGPUFractal3D :: (GPUFractal3D -> IO a) -> IO a
withGPUFractal3D f = do
    -- Create, compile and link shaders, load resources
    r <- runExceptT . runResourceT $ do
             gfVAO <- genObjectNameResource
             -- Load reflection environment map
             envStart <- liftIO getTick
             let reflMapFn = "./latlong_envmaps/uffizi-large.hdr"
             reflMap <- either throwError return =<< liftIO (loadHDRImage reflMapFn)
                        -- Build debug environment map
                        -- return buildTestLatLongEnvMap
             -- Build / verify cache of pre-convolved environment maps
             let powers = [1, 8, 64, 512]
                 mkCacheFn pow | pow == 0  = reflMapFn
                               | otherwise = dropExtension reflMapFn ++
                                             "_cache_pow_" ++ show pow ++ ".hdr"
                 powfn = map (\pow -> (pow, mkCacheFn pow)) powers -- Power / cache file nm. pairs
             liftIO $ buildPreConvolvedHDREnvMapCache reflMap powfn
             -- Load pre-convolved environment maps, convert to cube maps and upload to GPU
             let convertAndAllocCM envMap =
                     snd <$> allocate (latLongHDREnvMapToCubeMap envMap False) GL.deleteObjectName
             envCubeMaps <- forM powfn $ \(pow, fn) -> do
                 envMap <- either throwError return =<< liftIO (loadHDRImage fn)
                 tex    <- convertAndAllocCM envMap
                 return ( "env_cos_" ++ show (round pow :: Int) -- Shader uniform name
                        , tex                                   -- Cube map texture
                        )
             -- Add regular reflection environment map and store in record
             reflCubeMap <- convertAndAllocCM reflMap
             let gfEnvCubeMaps = ("env_reflection", reflCubeMap) : envCubeMaps
             envEnd <- liftIO getTick
             -- Create fragment shaders
             [gfDECornellBoxShd, gfDETestShd, gfMBPower8Shd, gfMBGeneralShd] <-
                 forM ([0..3] :: [Int]) $ \_ ->
                     snd <$> allocate GL.createProgram GL.deleteObjectName
             -- Cornell box geometry texture
             gfCornellBoxGeomTex <-
                snd <$> allocate mkCornellBoxVerticesTex GL.deleteObjectName
             -- Record
             let gpuFractal3D = GPUFractal3D { .. }
             -- Shaders
             shaderTime <- either throwError return
                               =<< liftIO (loadAndCompileShaders gpuFractal3D)
             -- Statistics
             liftIO . traceS TLInfo $ printf
                "withGPUFractal3D - Shader time: %.2fs, EnvMap time: %.2fs"
                shaderTime (envEnd - envStart)
             liftIO $ f gpuFractal3D
    either (traceAndThrow . printf "withGPUFractal3D - Init failed:\n%s") return r

loadAndCompileShaders :: GPUFractal3D -> IO (Either String Double)
loadAndCompileShaders GPUFractal3D { .. } = runExceptT $ do
    -- Fragment shader is loaded from a file
    shaderStart <- liftIO getTick
    fsSrc <- either (\(e :: IOException) -> throwError $ show e) return
                 =<< (liftIO . try . B.readFile $ "./fractal_3d.shd")
    -- Generate several shader variations through GLSL's pre-processor
    forM_ [ (gfDECornellBoxShd, "#define CORNELL_BOX_SCENE"                 )
          , (gfDETestShd      , ""                                          )
          , (gfMBPower8Shd    , "#define MANDELBULB_SCENE\n#define POWER8\n")
          , (gfMBGeneralShd   , "#define MANDELBULB_SCENE\n"                )
          ]
          $ \(shd, defines) ->
                let src = "#version 330 core\n" <> defines <> fsSrc
                 in either throwError return =<<
                        liftIO (compileShaderProgram vsSrcFSQuad src [] shd)
    shaderEnd <- liftIO getTick
    return $ shaderEnd - shaderStart -- Return shader load, compile and link time

buildPreConvolvedHDREnvMapCache :: JP.Image JP.PixelRGBF -> [(Float, FilePath)] -> IO ()
buildPreConvolvedHDREnvMapCache reflMap powfn = do
    -- Check if we have any pre-convolved files missing
    missing <- filterM (fmap not . doesFileExist . snd) powfn
    unless (null missing) $ do
        traceS TLInfo $ printf "Missing %i pre-convolved environment map(s), computing..."
            (length missing)
        -- We compute the pre-convolved versions from a small, downsampled reflection map
        (timeResized, resized) <- timeIt . evaluate . force $ resizeHDRImage reflMap 256
        traceS TLInfo $ printf "Downsampled reflection environment in %.2fs" timeResized
        -- Compute missing convolutions in parallel
        void $ flip mapConcurrently missing $ \(pow, fn) -> do
            (timeConvolved, convolved) <- timeIt . evaluate . force $
                cosineConvolveHDREnvMap resized pow
            traceS TLInfo $ printf "Computed power %.1f in %.2fs wallclock" pow timeConvolved
            (timeWritten, _) <- timeIt $ onException
                (JP.saveRadianceImage fn . JP.ImageRGBF $ convolved)
                (removeFile fn) -- Delete cache image file on error / cancellation
            traceS TLInfo $ printf "Written '%s' in %.2fs" (takeFileName fn) timeWritten

drawGPUFractal3D :: GPUFractal3D -> FractalShader -> Int -> Int -> Double -> IO ()
drawGPUFractal3D GPUFractal3D { .. } shdEnum w h time = do
    -- We need a dummy VAO active with all vertex attributes disabled
    GL.bindVertexArrayObject GL.$= Just gfVAO
    -- Bind shader
    let shd = case shdEnum of
                  FSDECornellBoxShader -> gfDECornellBoxShd
                  FSDETestShader       -> gfDETestShd
                  FSMBPower8Shader     -> gfMBPower8Shd
                  FSMBGeneralShader    -> gfMBGeneralShd
    GL.currentProgram GL.$= Just shd
    -- Setup uniforms
    let uniformFloat nm val =
            GL.get (GL.uniformLocation shd nm) >>= \(GL.UniformLocation loc) ->
                GLR.glUniform1f loc val
     in do uniformFloat "in_screen_wdh" $ fromIntegral w
           uniformFloat "in_screen_hgt" $ fromIntegral h
           uniformFloat "in_time"       $ realToFrac time
    -- Setup environment cube maps
    forM_ (zip gfEnvCubeMaps ([0..] :: [Int])) $ \((uniformName, tex), tuIdx) -> do
        setTextureShader tex GL.TextureCubeMap tuIdx shd uniformName
    -- Cornell box geometry texture
    setTextureShader gfCornellBoxGeomTex GL.Texture1D (length gfEnvCubeMaps) shd "cornell_geom"
    -- Don't need any VBO etc, the vertex shader will make this a proper quad.
    -- Specify one dummy attribute, as some drivers apparently have an issue
    -- with this otherwise (http://stackoverflow.com/a/8041472/1898360)
    GLR.glVertexAttrib1f 0 0
    -- Draw the full screen quad in tiles to prevent shader timeouts when we're rendering
    -- very complex images or very high resolution
    let tilesx = 1 :: Int
        tilesy = 1 :: Int
    forM_ [0..tilesy - 1] $ \y -> forM_ [0..tilesx - 1] $ \x -> do
        GL.get (GL.uniformLocation shd "quad") >>= \(GL.UniformLocation loc) ->
            GLR.glUniform4f loc
                (-1 + fromIntegral  x      / fromIntegral tilesx * 2)
                (-1 + fromIntegral  y      / fromIntegral tilesy * 2)
                (-1 + fromIntegral (x + 1) / fromIntegral tilesx * 2)
                (-1 + fromIntegral (y + 1) / fromIntegral tilesy * 2)
        GL.drawArrays GL.TriangleStrip 0 4

