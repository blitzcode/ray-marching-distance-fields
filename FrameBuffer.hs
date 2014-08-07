
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FrameBuffer ( withFrameBuffer
                   , fillFrameBuffer
                   , drawFrameBuffer
                   , saveFBToPNG
                   , FrameBuffer
                   ) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import Data.Word
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Codec.Picture as JP

import GLHelpers
import QuadRendering
import Trace

-- Simple 'frame buffer' interface where we can directly write into an RGBA8 vector and have
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
           GL.textureBinding GL.Texture2D GL.$= Just fbTex
           setTextureFiltering TFNone
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
          -- Prevent stalls by just allocating new PBO storage every time
       in bindPBO >> allocPBO fb >> GL.withMappedBuffer
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
      GL.textureBinding GL.Texture2D GL.$= Just fbTex
      -- TODO: Could use immutable textures through glTexStorage + glTexSubImage
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
    return r

drawFrameBuffer :: FrameBuffer -> QuadRenderBuffer -> IO ()
drawFrameBuffer FrameBuffer { .. } qb =
    -- Draw full screen quad 
    drawQuad qb
             0 0 (fromIntegral fbWdh) (fromIntegral fbHgt)
             10
             FCWhite
             TRNone
             (Just fbTex)
             QuadUVDefault

fbSize :: Integral a => Int -> Int -> a
fbSize w h = fromIntegral $ w * h * sizeOf (0 :: Word32)

-- Allocate new frame buffer sized backing storage for the bound PBO
allocPBO :: FrameBuffer -> IO ()
allocPBO FrameBuffer { .. } =
    GL.bufferData GL.PixelUnpackBuffer GL.$= ( fbSize fbWdh fbHgt -- In bytes
                                             , nullPtr            -- Just allocate
                                             , GL.StreamDraw      -- Dynamic
                                             )

saveFBToPNG :: FrameBuffer -> FilePath -> IO ()
saveFBToPNG FrameBuffer { .. } fn = do
    GL.textureBinding GL.Texture2D GL.$= Just fbTex
    img <- VSM.new $ fbSize fbWdh fbHgt :: IO (VSM.IOVector JP.Pixel8)
    (tw, th) <- getCurTex2DSize
    (tw == fbWdh && th == fbHgt) `assert` VSM.unsafeWith img $
        GL.getTexImage GL.Texture2D 0 . GL.PixelData GL.RGBA GL.UnsignedByte
    GL.textureBinding GL.Texture2D GL.$= Nothing
    let flipAndFixA img' =
          JP.generateImage
            ( \x y -> case JP.pixelAt img' x (fbHgt - 1 - y) of
                          JP.PixelRGBA8 r g b _ -> JP.PixelRGBA8 r g b 0xFF
            ) fbWdh fbHgt
     in JP.savePngImage fn . JP.ImageRGBA8 . flipAndFixA . JP.Image fbWdh fbHgt =<< VS.freeze img
    traceS TLInfo $ "Saved screenshot of framebuffer to " ++ fn

