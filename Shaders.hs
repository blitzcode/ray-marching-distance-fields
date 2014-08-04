
module Shaders ( mkShaderProgram
               , setAttribArray
               , setTextureShader
               , setOrtho2DProjMatrix
               ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Data.ByteString as B
import Data.Either
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

import GLHelpers

-- GLSL shaders and support functions

mkShaderProgram :: B.ByteString
                -> B.ByteString
                -> [(String, GL.AttribLocation)]
                -> IO (Either String GL.Program)
mkShaderProgram vsSrc fsSrc attribLocations =
    -- Always delete the shaders (don't need them after linking), only delete the program
    -- on error
    bracket        (GL.createShader GL.VertexShader  ) (GL.deleteObjectName) $ \shdVtx  ->
    bracket        (GL.createShader GL.FragmentShader) (GL.deleteObjectName) $ \shdFrag ->
    bracketOnError (GL.createProgram                 ) (GL.deleteObjectName) $ \shdProg -> do
        r <- runExceptT $ do
                 compile shdVtx  vsSrc
                 compile shdFrag fsSrc
                 liftIO $ GL.attachShader shdProg shdVtx >> GL.attachShader shdProg shdFrag
                 -- Need to specify attribute locations before we link
                 liftIO . forM_ attribLocations $
                     \(name, loc) -> GL.attribLocation shdProg name GL.$= loc
                 link shdProg
                 liftIO $ GL.detachShader shdProg shdVtx >> GL.detachShader shdProg shdFrag
                 return shdProg
        -- The bracket only deletes in case of an exception, still need to delete manually
        -- in case of a monadic error
        when (null $ rights [r]) $ GL.deleteObjectName shdProg
        traceOnGLError $ Just "mkShaderProgam end"
        return r
    -- Compile and link helpers
    where compile shd src = do
              liftIO $ do GL.shaderSourceBS shd GL.$= src
                          GL.compileShader  shd
              success <- liftIO $ GL.get $ GL.compileStatus shd
              unless success $ do
                  errLog <- liftIO $ GL.get $ GL.shaderInfoLog shd
                  throwError errLog
          link prog = do
              liftIO $ GL.linkProgram prog
              success <- liftIO $ GL.get $ GL.linkStatus prog
              unless success $ do
                  errLog <- liftIO $ GL.get $ GL.programInfoLog prog
                  throwError errLog

setAttribArray :: GL.GLuint
               -> Int
               -> Int
               -> Int
               -> IO GL.AttribLocation
setAttribArray idx attribStride vertexStride offset = do
    -- Specify and enable vertex attribute array
    let attrib = GL.AttribLocation idx
        szf    = sizeOf (0 :: Float)
    GL.vertexAttribPointer attrib GL.$=
        ( GL.ToFloat
        , GL.VertexArrayDescriptor
              (fromIntegral attribStride)
              GL.Float
              (fromIntegral $ vertexStride * szf)
              (nullPtr `plusPtr` (offset * szf))
        )
    GL.vertexAttribArray attrib GL.$= GL.Enabled
    return attrib

setTextureShader :: GL.TextureObject -> Int -> GL.Program -> String -> IO ()
setTextureShader tex tu prog uname = do
    (GL.get $ GL.uniformLocation prog uname) >>= \loc ->
        GL.uniform loc             GL.$= GL.Index1 (fromIntegral tu :: GL.GLint)
    GL.activeTexture               GL.$= GL.TextureUnit (fromIntegral tu)
    GL.textureBinding GL.Texture2D GL.$= Just tex

setOrtho2DProjMatrix :: GL.Program -> String -> Int -> Int -> IO ()
setOrtho2DProjMatrix prog uniform w h = do
    GL.UniformLocation loc <- GL.get $ GL.uniformLocation prog uniform
    let ortho2D = [ 2 / fromIntegral w, 0, 0, -1,
                    0, 2 / fromIntegral h, 0, -1,
                    0, 0, (-2) / 1000, -1, 
                    0, 0, 0, 1
                  ] :: [GL.GLfloat]
    withArray ortho2D $ \ptr -> GLR.glUniformMatrix4fv loc 1 1 {- transpose -} ptr

