
{-# LANGUAGE RecordWildCards, RankNTypes, TemplateHaskell, BangPatterns #-}

module App ( AppState(..)
           , AppEnv(..)
           , AppT
           , runAppT
           , run
           ) where

import Control.Lens
import Control.Loop
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Trans.Control
import Control.Concurrent.STM.TQueue
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import Data.Word
import Data.List
import Data.Complex
import Data.Bits
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import GLFWHelpers
import GLHelpers
import GLImmediate
import Timing
import Trace
import Font
import FrameBuffer
import qualified BoundedSequence as BS

data AppState = AppState { _asCurTick      :: !Double
                         , _asLastEscPress :: !Double
                         , _asFrameTimes   :: BS.BoundedSequence Double
                         }

data AppEnv = AppEnv { _aeWindow          :: GLFW.Window
                     , _aeGLFWEventsQueue :: TQueue GLFWEvent
                     , _aeFontTexture     :: GL.TextureObject
                     , _aeFB              :: FrameBuffer
                     }

makeLenses ''AppState
makeLenses ''AppEnv

-- Our application runs in a reader / state / IO transformer stack

type AppT m = StateT AppState (ReaderT AppEnv m)
type AppIO = AppT IO

runAppT :: Monad m => AppState -> AppEnv -> AppT m a -> m a
runAppT s e f = flip runReaderT e . flip evalStateT s $ f

processAllEvents :: MonadIO m => TQueue a -> (a -> m ()) -> m ()
processAllEvents tq processEvent = do
    me <- liftIO . atomically $ tryReadTQueue tq
    case me of
        Just e -> processEvent e >> processAllEvents tq processEvent
        _ -> return ()

processGLFWEvent :: GLFWEvent -> AppIO ()
processGLFWEvent ev =
    case ev of
        GLFWEventError e s -> do
           window <- view aeWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey win k {- sc -} _ ks {- mk -} _ ->
           when (ks == GLFW.KeyState'Pressed) $ do
               when (k == GLFW.Key'Escape) $ do
                   lastPress <- use asLastEscPress
                   tick      <- use asCurTick
                   -- Only close when ESC has been pressed twice quickly
                   when (tick - lastPress < 0.5) .
                       liftIO $ GLFW.setWindowShouldClose win True
                   asLastEscPress .= tick
        GLFWEventWindowSize {- win -} _ w h -> do
            -- TODO: Window resizing blocks event processing,
            -- see https://github.com/glfw/glfw/issues/1
            liftIO $ traceS TLInfo $ printf "Window resized: %i x %i" w h
        GLFWEventFramebufferSize {- win -} _ w h -> do
            liftIO $ setup2D w h
        {-
        GLFWEventMouseButton win bttn st mk -> do
            return ()
        GLFWEventCursorPos win x y -> do
            return ()
        GLFWEventScroll win x y -> do
            return ()
        -}
        _ -> return ()

{-# INLINE rasterMap #-}
rasterMap :: (Monad m)
          => Int -> Int -> (Int -> Int -> m ())
          -> m ()
rasterMap width height f = liner 0
  where liner y | y >= height = return ()
        liner y = columner 0
          where columner x | x >= width = liner (y + 1)
                columner x = f x y >> columner (x + 1)

draw :: AppIO ()
draw = do
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 1 0 1 1 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal

    --(w, h) <- (liftIO . GLFW.getFramebufferSize) =<< view aeWindow

    {-
    liftIO $ do

        -- GL.polygonMode GL.$= (GL.Line, GL.Line)

        tex <- GL.genObjectName
        GL.texture        GL.Texture2D GL.$= GL.Enabled
        GL.textureBinding GL.Texture2D GL.$= Just tex
        GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest, Nothing), GL.Nearest)

        let fbSize = w * h * sizeOf (0 :: Word32)
            size   = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
-}

        {-
        fb <- VSM.new $ w * h :: IO (VSM.IOVector Word32)

        forM_ [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] $ \(px, py) ->
            let idx = px + py * w
                x = ((fromIntegral px / fromIntegral w)) * 2.5 - 2 :: Float
                y = ((fromIntegral py / fromIntegral h)) * 2   - 1 :: Float
                c = x :+ y
                i = foldl' (\a _ -> a * a + c) (0 :+ 0) [1..100]
             in VSM.write fb idx $ if (realPart i) < 2 then 0x00FF0000 else 0x0000FF00 -- ABGR

        let size   = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
            texImg = GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 . GL.PixelData GL.RGBA GL.UnsignedByte
        VSM.unsafeWith fb $ texImg
        -}

        {-
        GL.windowPos (GL.Vertex2 0 0 :: GL.Vertex2 GL.GLint)
        assert (w * h == VSM.length fb) . VSM.unsafeWith fb $
            GL.drawPixels (GL.Size (fromIntegral w) (fromIntegral h))
                          . GL.PixelData GL.RGBA GL.UnsignedByte
        -}

        {-
        GL.renderPrimitive GL.Quads $ do
            texCoord2f 0 0
            vertex3f   0 0 (-1)
            texCoord2f 1 0
            vertex3f   (fromIntegral w) 0 (-1)
            texCoord2f 1 1
            vertex3f   (fromIntegral w) (fromIntegral h) (-1)
            texCoord2f 0 1
            vertex3f   0 (fromIntegral h) (-1)

        GL.texture GL.Texture2D GL.$= GL.Disabled
        GL.deleteObjectName tex
        -}

    fb <- view aeFB

    void $ liftIO $ fillFrameBuffer fb $ \w h fb -> do
        -- 9.6 10.3 FPS
        --forM_ [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] $ \(px, py) ->

        -- 2.7 10.7 FPS
        --numLoop 0 (h - 1) $ \py -> numLoop 0 (w - 1) $ \px ->

        -- 9.6 11.0 FPS
        forLoop 0 (< h) (+1) $ \py -> forLoop 0 (< w) (+1) $ \px ->

        -- 10.7 10.8 FPS
        --forM_ [0..h - 1] $ \py -> forM_ [0..w - 1] $ \px ->

        -- 9.4 10.3 FPS
        -- https://github.com/Twinside/Juicy.Pixels/blob/395f9cd18ac936f769fc63781f67c076637cf7aa/src/Codec/Picture/Jpg/Common.hs#L179
        --rasterMap w h $ \px py ->
            let idx = px + py * w
                x = ((fromIntegral px / fromIntegral w)) * 2.5 - 2 :: Float
                y = ((fromIntegral py / fromIntegral h)) * 2   - 1 :: Float
                c = x :+ y
                maxIter = 30
                (icnt, escC) = go (0 :: Int) (0 :+ 0)
                go iter z | (iter == maxIter) || (realPart z * realPart z + imagPart z * imagPart z > 4*4) = (iter, z)
                          | otherwise = let newZ = z * z + c
                                         in if newZ == z then (maxIter, z) else go (iter + 1) newZ
                icntCont = if icnt == maxIter then fromIntegral maxIter else fromIntegral icnt - (log(log(magnitude escC))) / log(2)
             --in VSM.unsafeWrite fb idx $ (truncate ((fromIntegral icnt) / fromIntegral maxIter * 255 :: Float) :: Word32) `shiftL` 8
             in VSM.unsafeWrite fb idx $ (truncate (icntCont / fromIntegral maxIter * 255 :: Float) :: Word32) `shiftL` 8

{-
       float modulus = sqrt (ReZ*ReZ + ImZ*ImZ);
      float mu = iter_count - (log (log (modulus)))/ log (2.0);
-}
        {-
        forM_ [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] $ \(px, py) ->
            let idx = px + py * w
                x = ((fromIntegral px / fromIntegral w)) * 2.5 - 2 :: Float
                y = ((fromIntegral py / fromIntegral h)) * 2   - 1 :: Float
                c = x :+ y
                i = foldl' (\a _ -> a * a + c) (0 :+ 0) [1..100]
             in liftIO . VSM.write fb idx $ if (realPart i) < 2 then 0x00FF0000 else 0x00007F00 -- ABGR
        -}

    liftIO $ drawFrameBuffer fb

    updateAndDrawFrameTimes

updateAndDrawFrameTimes :: AppIO ()
updateAndDrawFrameTimes = do
    frameTimes <- use $ asFrameTimes.to BS.toList
    curTick    <- use asCurTick
    asFrameTimes %= BS.push_ curTick
    let frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
        goFD prev (x:xs) = (prev - x) : goFD x xs
        goFD _ []        = []
        fdMean           = sum frameDeltas / (fromIntegral $ length frameDeltas)
        fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
        fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
        stats            =
          printf
            "%.1fFPS/%.1fms (Worst: %.1fFPS, Best: %.1fFPS)"
            (1.0 / fdMean ) (fdMean  * 1000)
            (1.0 / fdWorst)
            (1.0 / fdBest )
    fontTex <- view aeFontTexture
    liftIO $ drawText fontTex 4 0 0x00000000 stats
    liftIO $ drawText fontTex 3 1 0x00FFFFFF stats

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- view aeWindow
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize window
        GLFW.swapInterval 0
        setup2D w h
    -- Main loop
    let loop = do
          time <- liftIO $ getTick
          asCurTick .= time
          -- GLFW / OpenGL
          draw
          liftIO $ {-# SCC swapAndPoll #-} do
              -- GL.flush
              -- GL.finish
              GLFW.swapBuffers window
              GLFW.pollEvents
              traceOnGLError $ Just "main loop"
          tqGLFW <- view aeGLFWEventsQueue
          processAllEvents tqGLFW processGLFWEvent
          -- Done?
          flip unless loop =<< liftIO (GLFW.windowShouldClose window)
     in loop

