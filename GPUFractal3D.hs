
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module GPUFractal3D ( withGPUFractal3D
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

data GPUFractal3D = GPUFractal3D { gfVAO          :: !GL.VertexArrayObject
                                 , gfDETestShd    :: !GL.Program
                                 , gfMBPower8Shd  :: !GL.Program
                                 , gfMBGeneralShd :: !GL.Program
                                 , gfEnvCubeMaps  :: [(String, GL.TextureObject)]
                                 }

data FractalShader = FSDETestShader | FSMBPower8Shader | FSMBGeneralShader
                     deriving (Show, Eq, Enum)

withGPUFractal3D :: (GPUFractal3D -> IO a) -> IO a
withGPUFractal3D f = do
    -- Create, compile and link shaders, load resources
    r <- runExceptT . runResourceT $ do
             gfVAO <- genObjectNameResource
             -- Fragment shader is loaded from a file
             shaderStart <- liftIO getTick
             fsSrc <- either (\(e :: IOException) -> throwError $ show e) return
                 =<< (liftIO . try . B.readFile $ "./fractal_3d.shd")
             -- Generate several shader variations through GLSL's pre-processor
             [gfDETestShd, gfMBPower8Shd, gfMBGeneralShd] <-
                forM [ ""
                     , "#define MANDELBULB_SCENE\n#define POWER8\n"
                     , "#define MANDELBULB_SCENE\n"
                     ]
                     $ \defines -> let src = "#version 330 core\n" <> defines <> fsSrc
                                    in tryMkShaderResource $ mkShaderProgram vsSrcFSQuad src []
             shaderEnd <- liftIO getTick
             envStart <- liftIO getTick
             -- Load reflection environment map
             let reflMapFn = "./latlong_envmaps/uffizi_gallery.hdr"
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
             -- Statistics
             envEnd <- liftIO getTick
             liftIO . traceS TLInfo $ printf
                "withGPUFractal3D - Shader time: %.2fs, EnvMap time: %.2fs"
                (shaderEnd - shaderStart) (envEnd - envStart)
             liftIO $ f GPUFractal3D { .. }
    either (traceAndThrow . printf "withGPUFractal3D - Init failed:\n%s") return r

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
                  FSDETestShader    -> gfDETestShd
                  FSMBPower8Shader  -> gfMBPower8Shd
                  FSMBGeneralShader -> gfMBGeneralShd
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
    -- Draw fullscreen quad. Don't need any VBO etc, the vertex shader will make this a
    -- proper quad. Specify one dummy attribute, as some drivers apparently have an issue
    -- with this otherwise (http://stackoverflow.com/a/8041472/1898360)
    GLR.glVertexAttrib1f 0 0
    GL.drawArrays GL.TriangleStrip 0 4

