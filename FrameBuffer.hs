
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FrameBuffer ( withFrameBuffer
                   , fillFrameBuffer
                   , drawFrameBuffer
                   , FrameBuffer
                   ) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import Data.Word
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import GLHelpers
import GLImmediate
import Trace

-- Simple 'frame buffer' interface where we can directly write into am ABGR8 vector and have
-- it appear on screen. Uses PBOs to allow writes to a texture drawn as a full screen quad

data FrameBuffer = FrameBuffer { fbTex :: !GL.TextureObject
                               , fbPBO :: !GL.BufferObject
                               , fbWdh :: !Int
                               , fbHgt :: !Int
                               }

withFrameBuffer :: Int -> Int -> (FrameBuffer -> IO a) -> IO a
withFrameBuffer fbWdh fbHgt f = do
  traceOnGLError $ Just "withFrameBuffer begin"
  r <- bracket GL.genObjectName GL.deleteObjectName $ \fbTex ->
         bracket GL.genObjectName GL.deleteObjectName $ \fbPBO -> do
           GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just fbPBO
           let fb = FrameBuffer { .. }
           allocPBO fb
           GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
           traceOnGLError $ Just "withFrameBuffer begin inner"
           f fb
  traceOnGLError $ Just "withFrameBuffer after cleanup"
  return r

fillFrameBuffer :: (MonadBaseControl IO m, MonadIO m)
                => FrameBuffer
                -> (Int -> Int -> VSM.MVector s Word32 -> m a) -- Run inner inside the base monad
                -> m (Maybe a)                                 -- We return Nothing if mapping fails
fillFrameBuffer fb@(FrameBuffer { .. }) f = do
    -- Map. If this function is nested inside another fillFrameBuffer with the same FrameBuffer,
    -- the mapping operation will fail as OpenGL does not allow two concurrent mappings. Hence,
    -- no need to check for this explicitly
    r <- control $ \run -> liftIO $ do
      let bindPBO = GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just fbPBO
      bindPBO
      allocPBO fb -- Prevent stalls by just allocating new PBO storage every time
      GL.withMappedBuffer
            GL.PixelUnpackBuffer
            GL.WriteOnly
            ( \ptrPBO -> newForeignPtr_ ptrPBO >>= \fpPBO ->
                finally
                  ( -- Run in outer base monad
                    run $ Just <$> f fbWdh fbHgt
                      (VSM.unsafeFromForeignPtr0 fpPBO $ fbSize fbWdh fbHgt)
                  )
                  bindPBO -- Make sure we rebind our PBO, otherwise
                          -- unmapping might fail if the inner
                          -- modified the bound buffer objects
            )
            ( \mf -> do traceS TLError $ "fillFrameBuffer - PBO mapping failure: " ++ show mf
                        run $ return Nothing
            )
    liftIO $ do
      -- Update frame buffer texture from the PBO data
      GL.texture        GL.Texture2D GL.$= GL.Enabled
      GL.textureBinding GL.Texture2D GL.$= Just fbTex
      liftIO $ GL.texImage2D GL.Texture2D
                             GL.NoProxy
                             0
                             GL.RGBA8
                             (GL.TextureSize2D (fromIntegral fbWdh) (fromIntegral fbHgt))
                             0
                             (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
      -- Done
      GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
      GL.textureBinding GL.Texture2D GL.$= Nothing
      GL.texture        GL.Texture2D GL.$= GL.Disabled
    return r

drawFrameBuffer :: FrameBuffer -> IO ()
drawFrameBuffer FrameBuffer { .. } = do
    -- Setup texture
    GL.texture        GL.Texture2D GL.$= GL.Enabled
    GL.textureBinding GL.Texture2D GL.$= Just fbTex
    GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest, Nothing), GL.Nearest)
    -- Draw full screen quad 
    GL.renderPrimitive GL.Quads $ do
        texCoord2f 0 0
        vertex3f   0 0 (-1)
        texCoord2f 1 0
        vertex3f   (fromIntegral fbWdh) 0 (-1)
        texCoord2f 1 1
        vertex3f   (fromIntegral fbWdh) (fromIntegral fbHgt) (-1)
        texCoord2f 0 1
        vertex3f   0 (fromIntegral fbHgt) (-1)
    -- Done
    GL.textureBinding GL.Texture2D GL.$= Nothing
    GL.texture GL.Texture2D        GL.$= GL.Disabled

fbSize :: Integral a => Int -> Int -> a
fbSize w h = fromIntegral $ w * h * sizeOf (0 :: Word32)

-- Allocate new frame buffer sized backing storage for the bound PBO
allocPBO :: FrameBuffer -> IO ()
allocPBO FrameBuffer { .. } =
    GL.bufferData GL.PixelUnpackBuffer GL.$= ( fbSize fbWdh fbHgt -- In bytes
                                             , nullPtr            -- Just allocate
                                             , GL.StreamDraw      -- Dynamic
                                             )

