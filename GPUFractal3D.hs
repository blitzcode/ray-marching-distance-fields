
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module GPUFractal3D ( withGPUFractal3D
                    , GPUFractal3D
                    , drawGPUFractal3D
                    ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Text.Printf
import qualified Graphics.Rendering.OpenGL as GL

import Trace
import Shaders
import GPUFractal3DShaderSource

data GPUFractal3D = GPUFractal3D { gfTestShd :: !GL.Program
                                 }

withGPUFractal3D :: (GPUFractal3D -> IO a) -> IO a
withGPUFractal3D f = do
    -- Create, compile and link shaders
    r <- runExceptT . runResourceT $ do
             gfTestShd <- tryMkShaderResource $ mkShaderProgram vsSrcFSQuad fsSrcBasic []
             liftIO $ f GPUFractal3D { .. }
    either (traceAndThrow . printf "withGPUFractal3D - Shader init failed:\n%s") return r

drawGPUFractal3D :: GPUFractal3D -> IO ()
drawGPUFractal3D GPUFractal3D { .. } = do
    GL.bindVertexArrayObject GL.$= Nothing
    GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
    GL.currentProgram GL.$= Just gfTestShd
    -- Draw fullscreen quad. Don't need any VBO etc, the vertex shader will make this a
    -- proper quad
    GL.drawArrays GL.TriangleStrip 0 4

