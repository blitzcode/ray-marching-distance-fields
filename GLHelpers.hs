
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module GLHelpers ( getGLStrings
                 , traceOnGLError
                 , throwOnGLError
                 , getCurTex2DSize
                 , disableVAOAndShaders
                 , Transparency(..)
                 , setTransparency
                 , setTextureFiltering
                 , setTextureClampST
                 , TextureFiltering(..)
                 , setupViewport
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad
import Control.Exception
import Text.Printf
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import Trace

-- Various utility functions related to OpenGL

getErrors :: Maybe String -> IO (Maybe String)
getErrors context =
    GL.get GL.errors >>= \case
        []  -> return Nothing
        err -> return . Just $
                   "OpenGL Error" ++ maybe ": " (\c -> " (" ++ c ++ "): ") context ++ show err

traceOnGLError :: Maybe String -> IO ()
traceOnGLError context = getErrors context >>= maybe (return ()) (traceS TLError)

throwOnGLError :: Maybe String -> IO ()
throwOnGLError context = getErrors context >>= maybe (return ()) (throwIO . ErrorCall)

getGLStrings :: IO String
getGLStrings = do
  -- No wrapper around the OpenGL 3 extension APIs yet, have to use the raw ones
  numExt <-
      alloca $ \(ptr :: Ptr GLR.GLint) -> GLR.glGetIntegerv GLR.gl_NUM_EXTENSIONS ptr >> peek ptr
  printf
      "OpenGL - Vendor: %s · Renderer: %s · Version: %s · GLSL: %s · Num Extensions: %i · GLFW: %s"
      <$> GL.get GL.vendor
      <*> GL.get GL.renderer
      <*> GL.get GL.glVersion
      <*> GL.get GL.shadingLanguageVersion
      <*> (pure $ fromIntegral numExt :: IO Int)
      <*> (fromJust <$> GLFW.getVersionString)

getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\(GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D GL.Texture2D 0)

data TextureFiltering = TFNone | TFMinMag | TFMinOnly | TFMagOnly

setTextureFiltering :: TextureFiltering -> IO ()
setTextureFiltering TFNone =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing        ), GL.Nearest)
setTextureFiltering TFMinMag =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
setTextureFiltering TFMinOnly =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Nearest)
setTextureFiltering TFMagOnly =
    GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing        ), GL.Linear')

setTextureClampST :: IO ()
setTextureClampST =
    forM_ [GL.S, GL.T] $
        \x -> GL.textureWrapMode GL.Texture2D x GL.$= (GL.Repeated, GL.ClampToEdge)

data Transparency = TRNone
                  | TRBlend !Float
                  | TRSrcAlpha
                  deriving (Eq, Ord, Show)

setTransparency :: Transparency -> IO ()
setTransparency trans =
    case trans of TRNone -> GL.blend GL.$= GL.Disabled
                  TRBlend weight -> do
                      GL.blend      GL.$= GL.Enabled
                      GL.blendFunc  GL.$= (GL.ConstantAlpha, GL.OneMinusConstantAlpha)
                      GL.blendColor GL.$= GL.Color4 0 0 0 (realToFrac weight :: GL.GLfloat)
                  TRSrcAlpha -> do
                      GL.blend     GL.$= GL.Enabled
                      GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

-- Disable vertex attribute arrays and shaders
disableVAOAndShaders :: IO ()
disableVAOAndShaders = do
    GL.bindVertexArrayObject GL.$= Nothing
    GL.currentProgram        GL.$= Nothing

setupViewport :: Int -> Int -> IO ()
setupViewport w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

