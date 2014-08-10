
{-# LANGUAGE RecordWildCards #-}

module GPUFractal3D ( withGPUFractal3D
                    , GPUFractal3D
                    , drawGPUFractal3D
                    ) where

import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Text.Printf
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR

import Trace
import GLHelpers
import Shaders
import GPUFractal3DShaderSource

data GPUFractal3D = GPUFractal3D { gfVAO     :: !GL.VertexArrayObject
                                 , gfTestShd :: !GL.Program
                                 }

withGPUFractal3D :: (GPUFractal3D -> IO a) -> IO a
withGPUFractal3D f = do
    -- Create, compile and link shaders
    r <- runExceptT . runResourceT $ do
             gfVAO     <- genObjectNameResource
             gfTestShd <- tryMkShaderResource $ mkShaderProgram vsSrcFSQuad fsSrcBasic []
             liftIO $ f GPUFractal3D { .. }
    either (traceAndThrow . printf "withGPUFractal3D - Shader init failed:\n%s") return r

drawGPUFractal3D :: GPUFractal3D -> Int -> Int -> Double -> IO ()
drawGPUFractal3D GPUFractal3D { .. } w h time = do
    -- We need a dummy VAO active with all vertex attributes disabled
    GL.bindVertexArrayObject GL.$= Just gfVAO
    -- Setup shader
    GL.currentProgram        GL.$= Just gfTestShd
    GL.get (GL.uniformLocation gfTestShd "in_aspect") >>= \(GL.UniformLocation loc) ->
        GLR.glUniform1f loc (fromIntegral w / fromIntegral h)
    GL.get (GL.uniformLocation gfTestShd "in_time"  ) >>= \(GL.UniformLocation loc) ->
        GLR.glUniform1f loc (realToFrac time)
    -- Draw fullscreen quad. Don't need any VBO etc, the vertex shader will make this a
    -- proper quad. Specify one dummy attribute, as some drivers apparently have an issue
    -- with this otherwise (http://stackoverflow.com/a/8041472/1898360)
    GLR.glVertexAttrib1f 0 0
    GL.drawArrays GL.TriangleStrip 0 4

