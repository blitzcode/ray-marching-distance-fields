
{-# LANGUAGE LambdaCase #-}

module GLHelpers ( setup2D
                 , getGLStrings
                 , traceOnGLError
                 , throwOnGLError
                 , getCurTex2DSize
                 ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Exception
import Text.Printf
import Data.Maybe

import Trace

-- Various utility functions related to OpenGL

setup2D :: Int -> Int -> IO ()
setup2D w h = do
    GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity
    -- GLU.ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
    GL.ortho 0 (fromIntegral w) 0 (fromIntegral h) 0 1000

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
getGLStrings =
  printf
    "OpenGL - Vendor: %s · Renderer: %s · Version: %s · GLSL: %s · Num Extensions: %i · GLFW: %s"
    <$> GL.get GL.vendor
    <*> GL.get GL.renderer
    <*> GL.get GL.glVersion
    <*> GL.get GL.shadingLanguageVersion
    <*> (length <$> GL.get GL.glExtensions)
    <*> (fromJust <$> GLFW.getVersionString)
    -- <*> (show <$> GL.get GL.glExtensions)
    --
getCurTex2DSize :: IO (Int, Int)
getCurTex2DSize = (\(GL.TextureSize2D w h) -> (fromIntegral w, fromIntegral h))
                         <$> (GL.get $ GL.textureSize2D GL.Texture2D 0)

