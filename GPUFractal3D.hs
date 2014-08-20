
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module GPUFractal3D ( withGPUFractal3D
                    , GPUFractal3D
                    , drawGPUFractal3D
                    , FractalShader(..)
                    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR

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
                                 , gfEnvCubeMap   :: !GL.TextureObject
                                 }

data FractalShader = FSDETestShader | FSMBPower8Shader | FSMBGeneralShader
                     deriving (Show, Eq, Enum)

withGPUFractal3D :: (GPUFractal3D -> IO a) -> IO a
withGPUFractal3D f = do
    -- Create, compile and link shaders, load resources
    r <- runExceptT . runResourceT $ do
             gfVAO <- genObjectNameResource
             -- Build-in fragment shader can be overridden with a file
             shaderStart <- liftIO getTick
             fsSrc <- either (\(_ :: IOException) -> return fsSrcFractal) return
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
             -- Environment cube maps
             envStart <- liftIO getTick
             latlong <- either throwError return
                            =<< liftIO (loadHDRImage "./latlong_envmaps/uffizi_gallery.hdr")
             gfEnvCubeMap <- snd <$> allocate (latLongHDREnvMapToCubeMap latlong)
                                              GL.deleteObjectName
             envEnd <- liftIO getTick
             liftIO . traceS TLInfo $ printf
                "withGPUFractal3D - Shader time: %.2fs, EnvMap time: %.2fs"
                (shaderEnd - shaderStart) (envEnd - envStart)
             liftIO $ f GPUFractal3D { .. }
    either (traceAndThrow . printf "withGPUFractal3D - Init failed:\n%s") return r

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
    -- Setup textures
    (GL.get $ GL.uniformLocation shd "env") >>= \loc ->
        GL.uniform loc GL.$= GL.Index1 (0 :: GL.GLint)
    GL.activeTexture   GL.$= GL.TextureUnit 0
    GL.textureBinding GL.TextureCubeMap GL.$= Just gfEnvCubeMap
    -- Draw fullscreen quad. Don't need any VBO etc, the vertex shader will make this a
    -- proper quad. Specify one dummy attribute, as some drivers apparently have an issue
    -- with this otherwise (http://stackoverflow.com/a/8041472/1898360)
    GLR.glVertexAttrib1f 0 0
    GL.drawArrays GL.TriangleStrip 0 4

