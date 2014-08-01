
{-# LANGUAGE OverloadedStrings #-}

module Shaders ( vsSrcBasic
               , fsSrcBasic
               , fsColOnlySrcBasic
               , mkShaderProgam
               , setAttribArray
               , setTextureShader
               , setProjMatrixFromFFP
               ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

mkShaderProgam :: B.ByteString -> B.ByteString -> IO (Either String GL.Program)
mkShaderProgam vsSrc fsSrc =
    -- Always delete the shaders (don't need them after linking), only delete the program
    -- on error
    bracket        (GL.createShader GL.VertexShader  ) (GL.deleteObjectName) $ \shdVtx  ->
    bracket        (GL.createShader GL.FragmentShader) (GL.deleteObjectName) $ \shdFrag ->
    bracketOnError (GL.createProgram                 ) (GL.deleteObjectName) $ \shdProg -> do
        r <- runExceptT $ do
                 compile shdVtx  vsSrc
                 compile shdFrag fsSrc
                 liftIO $ GL.attachShader shdProg shdVtx >> GL.attachShader shdProg shdFrag
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

-- Hack to set a shader's uniform matrix from the FFP projection matrix
setProjMatrixFromFFP :: GL.Program -> String -> IO ()
setProjMatrixFromFFP prog uniform = do
    GL.UniformLocation loc <- GL.get $ GL.uniformLocation prog uniform
    withArray ([1..16] :: [GL.GLfloat]) $ \ptr -> do GLR.glGetFloatv GLR.gl_PROJECTION_MATRIX ptr
                                                     GLR.glUniformMatrix4fv loc 1 0 ptr

-- Shader source for basic vertex and fragment shaders
vsSrcBasic, fsSrcBasic, fsColOnlySrcBasic :: B.ByteString
vsSrcBasic = TE.encodeUtf8 $ T.unlines
    [ "#version 120"
    , "uniform mat4 in_mvp;"
    , "attribute vec3 in_pos;"
    , "attribute vec4 in_col;"
    , "attribute vec2 in_uv;"
    , "varying vec4 fs_col;"
    , "varying vec2 fs_uv;"
    , "void main()"
    , "{"
    , "    gl_Position = in_mvp * vec4(in_pos, 1.0);"
    , "    fs_col      = in_col;"
    , "    fs_uv       = in_uv;"
    , "}"
    ]
fsSrcBasic = TE.encodeUtf8 $ T.unlines
    [ "#version 120"
    , "varying vec4 fs_col;"
    , "varying vec2 fs_uv;"
    , "uniform sampler2D tex;"
    , "void main()"
    , "{"
    , "   gl_FragColor = fs_col * texture2D(tex, fs_uv);"
    , "}"
    ]
fsColOnlySrcBasic = TE.encodeUtf8 $ T.unlines
    [ "#version 120"
    , "varying vec4 fs_col;"
    , "void main()"
    , "{"
    , "   gl_FragColor = fs_col;"
    , "}"
    ]

