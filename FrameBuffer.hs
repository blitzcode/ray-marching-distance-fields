
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FrameBuffer ( withFrameBuffer
                   , fillFrameBuffer
                   , drawFrameBuffer
                   , saveFrameBufferToPNG
                   , resizeFrameBuffer
                   , getFrameBufferDim
                   , FrameBuffer
                   ) where

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import Data.Word
import Data.IORef
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
                               , fbDim :: IORef (Int, Int)
                               }

withFrameBuffer :: Int -> Int -> (FrameBuffer -> IO a) -> IO a
withFrameBuffer w h f = do
    traceOnGLError $ Just "withFrameBuffer begin"
    r <- bracket GL.genObjectName GL.deleteObjectName $ \fbTex ->
         bracket GL.genObjectName GL.deleteObjectName $ \fbPBO -> do
             GL.textureBinding GL.Texture2D GL.$= Just fbTex
             setTextureFiltering TFMagOnly -- Otherwise we'd need to use automatic MIP-map gen.
             setTextureClampST -- No wrap-around artifacts at the FB borders
             GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
             traceOnGLError $ Just "withFrameBuffer begin inner"
             fbDim <- newIORef (w, h)
             f FrameBuffer { .. }
    traceOnGLError $ Just "withFrameBuffer after cleanup"
    return r

resizeFrameBuffer :: FrameBuffer -> Int -> Int -> IO ()
resizeFrameBuffer fb w h = writeIORef (fbDim fb) (w, h)

getFrameBufferDim :: FrameBuffer -> IO (Int, Int)
getFrameBufferDim fb = readIORef $ fbDim fb

fillFrameBuffer :: (MonadBaseControl IO m, MonadIO m)
                => FrameBuffer
                -> (Int -> Int -> VSM.MVector s Word32 -> m a) -- Run inner inside the base monad
                -> m (Maybe a)                                 -- We return Nothing if mapping fails
fillFrameBuffer fb@(FrameBuffer { .. }) f = do
    -- Map. If this function is nested inside another fillFrameBuffer with the same FrameBuffer,
    -- the mapping operation will fail as OpenGL does not allow two concurrent mappings. Hence,
    -- no need to check for this explicitly
    (w, h) <- liftIO $ readIORef fbDim
    r <- control $ \run -> liftIO $ do
      let bindPBO = GL.bindBuffer GL.PixelUnpackBuffer GL.$= Just fbPBO
          -- Prevent stalls by just allocating new PBO storage every time
       in bindPBO >> allocPBO fb >> GL.withMappedBuffer
            GL.PixelUnpackBuffer
            GL.WriteOnly
            ( \ptrPBO -> newForeignPtr_ ptrPBO >>= \fpPBO ->
                finally
                  -- Run in outer base monad
                  ( run $ Just <$> f w h (VSM.unsafeFromForeignPtr0 fpPBO $ fbSizeB w h) )
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
                             (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
                             0
                             (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
      -- Done
      GL.bindBuffer GL.PixelUnpackBuffer GL.$= Nothing
      GL.textureBinding GL.Texture2D GL.$= Nothing
    return r

drawFrameBuffer :: FrameBuffer -> QuadRenderBuffer -> Float -> Float -> Float -> Float -> IO ()
drawFrameBuffer FrameBuffer { .. } qb x1 y1 x2 y2  =
    -- Draw quad with frame buffer texture
    drawQuad qb
             x1 y1 x2 y2
             10
             FCWhite
             TRNone
             (Just fbTex)
             QuadUVDefault

fbSizeB :: Integral a => Int -> Int -> a
fbSizeB w h = fromIntegral $ w * h * sizeOf (0 :: Word32)

-- Allocate new frame buffer sized backing storage for the bound PBO
allocPBO :: FrameBuffer -> IO ()
allocPBO FrameBuffer { .. } = do
    (w, h) <- readIORef fbDim
    GL.bufferData GL.PixelUnpackBuffer GL.$= ( fbSizeB w h   -- In bytes
                                             , nullPtr       -- Just allocate
                                             , GL.StreamDraw -- Dynamic
                                             )

saveFrameBufferToPNG :: FrameBuffer -> FilePath -> IO ()
saveFrameBufferToPNG FrameBuffer { .. } fn = do
    GL.textureBinding GL.Texture2D GL.$= Just fbTex
    (w, h) <- getCurTex2DSize
    img    <- VSM.new $ fbSizeB w h :: IO (VSM.IOVector JP.Pixel8)
    VSM.unsafeWith img $ GL.getTexImage GL.Texture2D 0 . GL.PixelData GL.RGBA GL.UnsignedByte
    GL.textureBinding GL.Texture2D GL.$= Nothing
    let flipAndFixA img' =
          JP.generateImage
            ( \x y -> case JP.pixelAt img' x (h - 1 - y) of
                          JP.PixelRGBA8 r g b _ -> JP.PixelRGBA8 r g b 0xFF
            ) w h
     in JP.savePngImage fn . JP.ImageRGBA8 . flipAndFixA . JP.Image w h =<< VS.freeze img
    traceS TLInfo $ "Saved screenshot of framebuffer to " ++ fn

