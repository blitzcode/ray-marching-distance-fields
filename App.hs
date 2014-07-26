
{-# LANGUAGE RecordWildCards, RankNTypes, TemplateHaskell #-}

module App ( AppState(..)
           , AppEnv(..)
           , AppT
           , runAppT
           , run
           ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.ST
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Storable as VS
import Control.Concurrent.STM.TQueue
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import Data.Monoid

import GLFWHelpers
import GLHelpers
import Timing
import Trace
import Font
import qualified BoundedSequence as BS

data AppState = AppState { _asCurTick      :: !Double
                         , _asLastEscPress :: !Double
                         , _asFrameTimes   :: BS.BoundedSequence Double
                         }

data AppEnv = AppEnv { _aeWindow          :: GLFW.Window
                     , _aeGLFWEventsQueue :: TQueue GLFWEvent
                     , _aeFontTexture     :: GL.TextureObject
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

draw :: AppIO ()
draw = do
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 1 1 1 1 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal

    {-
    let toPixels = (\x -> if x then 0x00FFFFFF else 0x00000000) :: Bool -> Word32
        arr = VS.convert $ VU.map toPixels grid
        size = GL.Size (fromIntegral w) (fromIntegral h)
    assert (w * h == VS.length arr) $ VS.unsafeWith arr (\ptr ->
        GL.drawPixels size (GL.PixelData GL.RGBA GL.UnsignedByte ptr))

    let abc :: VS.Vector Int
        abc = runST $ do
                  v <- VSM.new 1024 :: forall s. ST s (VSM.MVector s Int)
                  VSM.write v 0 1000
                  VS.freeze v
    -}

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
    fontTex <- view aeFontTexture
    liftIO . drawText fontTex 3 1 0x00000000 $ printf
        "Mean: %.1fFPS/%.1fms | Worst: %.1fFPS/%.1fms | Best: %.1fFPS/%.1fms\n"
        (1.0 / fdMean ) (fdMean  * 1000)
        (1.0 / fdWorst) (fdWorst * 1000)
        (1.0 / fdBest ) (fdBest  * 1000)

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- view aeWindow
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize window
        GLFW.swapInterval 1
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

