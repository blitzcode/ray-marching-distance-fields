
{-# LANGUAGE RecordWildCards #-}

module App ( AppState(..)
           , AppEnv(..)
           , AppT
           , runAppT
           , run
           ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf

import GLFWHelpers
import GLHelpers
import Timing
import Trace
import Font
import qualified BoundedSequence as BS

data AppState = AppState { asCurTick      :: Double
                         , asLastEscPress :: Double
                         , asFrameTimes   :: BS.BoundedSequence Double
                         }

data AppEnv = AppEnv { aeWindow          :: GLFW.Window
                     , aeGLFWEventsQueue :: TQueue GLFWEvent
                     , aeFontTexture     :: GL.TextureObject
                     }

-- Our application runs in a reader / state / IO transformer stack

type AppT m a = StateT AppState (ReaderT AppEnv m) a
type AppIO a = AppT IO a

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
           window <- asks aeWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey win k {- sc -} _ ks {- mk -} _ ->
           when (ks == GLFW.KeyState'Pressed) $ do
               when (k == GLFW.Key'Escape) $ do
                   lastPress <- gets asLastEscPress
                   tick <- gets asCurTick
                   -- Only close when ESC has been pressed twice quickly
                   when (tick - lastPress < 0.5) .
                       liftIO $ GLFW.setWindowShouldClose win True
                   modify' $ \s -> s { asLastEscPress = tick }
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
    updateAndDrawFrameTimes

updateAndDrawFrameTimes :: AppIO ()
updateAndDrawFrameTimes = do
    AppEnv   { .. } <- ask
    AppState { .. } <- get
    modify' $ \s -> s { asFrameTimes = BS.push_ asCurTick asFrameTimes }
    let frameTimes       = BS.toList $ asFrameTimes
        frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
        goFD prev (x:xs) = (prev - x) : goFD x xs
        goFD _ []        = []
        fdMean           = sum frameDeltas / (fromIntegral $ length frameDeltas)
        fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
        fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
    liftIO . drawText aeFontTexture 3 1 0x00000000 $ printf
        "Mean: %.1fFPS/%.1fms | Worst: %.1fFPS/%.1fms | Best: %.1fFPS/%.1fms\n"
        (1.0 / fdMean ) (fdMean  * 1000)
        (1.0 / fdWorst) (fdWorst * 1000)
        (1.0 / fdBest ) (fdBest  * 1000)

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- asks aeWindow
    liftIO $ do
        (w, h) <- GLFW.getFramebufferSize window
        GLFW.swapInterval 1
        setup2D w h
    -- Main loop
    let loop = do
          time <- liftIO $ getTick
          modify' $ \s -> s { asCurTick = time }
          -- GLFW / OpenGL
          draw
          liftIO $ {-# SCC swapAndPoll #-} do
              GL.flush
              GL.finish
              GLFW.swapBuffers window
              GLFW.pollEvents
              traceOnGLError $ Just "main loop"
          tqGLFW <- asks aeGLFWEventsQueue
          processAllEvents tqGLFW processGLFWEvent
          -- Done?
          flip unless loop =<< liftIO (GLFW.windowShouldClose window)
     in loop

