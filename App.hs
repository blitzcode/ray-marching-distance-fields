
{-# LANGUAGE RecordWildCards, RankNTypes, TemplateHaskell, LambdaCase, FlexibleContexts #-}

module App ( AppState(..)
           , AppEnv(..)
           , Mode(..)
           , AppT
           , runAppT
           , run
           ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Control.Applicative
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Printf
import Data.Time

import GLFWHelpers
import GLHelpers
import Timing
import Trace
import Font
import FrameBuffer
import Fractal2D
import QuadRendering
import qualified BoundedSequence as BS

data Mode = ModeJuliaAnim | ModeJuliaAnimSmooth | ModeMandelBrot | ModeMandelBrotSmooth
            deriving (Enum, Eq, Bounded, Show)

data AppState = AppState { _asCurTick      :: !Double
                         , _asLastEscPress :: !Double
                         , _asFrameTimes   :: BS.BoundedSequence Double
                         , _asMode         :: !Mode
                         }

data AppEnv = AppEnv { _aeWindow          :: GLFW.Window
                     , _aeGLFWEventsQueue :: TQueue GLFWEvent
                     , _aeFontTexture     :: GL.TextureObject
                     , _aeFB              :: FrameBuffer
                     , _aeQR              :: QuadRenderer
                     }

makeLenses ''AppState
makeLenses ''AppEnv

-- Our application runs in a reader / state / IO transformer stack

type AppT m = StateT AppState (ReaderT AppEnv m)
type AppIO = AppT IO

runAppT :: Monad m => AppState -> AppEnv -> AppT m a -> m a
runAppT s e f = flip runReaderT e . flip evalStateT s $ f

processAllEvents :: MonadIO m => TQueue a -> (a -> m ()) -> m ()
processAllEvents tq processEvent =
    (liftIO . atomically $ tryReadTQueue tq) >>= \case
        Just e -> processEvent e >> processAllEvents tq processEvent
        _      -> return ()

processGLFWEvent :: GLFWEvent -> AppIO ()
processGLFWEvent ev =
    case ev of
        GLFWEventError e s -> do
           window <- view aeWindow
           liftIO $ do
               traceS TLError $ "GLFW Error " ++ show e ++ " " ++ show s
               GLFW.setWindowShouldClose window True
        GLFWEventKey win k {- sc -} _ ks {- mk -} _ | ks == GLFW.KeyState'Pressed ->
            case k of
                GLFW.Key'Escape -> do
                    lastPress <- use asLastEscPress
                    tick      <- use asCurTick
                    -- Only close when ESC has been pressed twice quickly
                    when (tick - lastPress < 0.5) .
                        liftIO $ GLFW.setWindowShouldClose win True
                    asLastEscPress .= tick
                -- Also clear frame time history on mode switch
                GLFW.Key'Minus -> asMode %= wrapPred >> asFrameTimes %= BS.clear
                GLFW.Key'Equal -> asMode %= wrapSucc >> asFrameTimes %= BS.clear
                GLFW.Key'S     -> view aeFB >>= \fb -> liftIO $ saveFBToPNG fb .
                                    map (\c -> if c `elem` ['/', '\\', ':', ' '] then '-' else c)
                                      . printf "Screenshot-%s.png" =<< show <$> getZonedTime
                _              -> return ()
        GLFWEventWindowSize {- win -} _ w h -> do
            -- TODO: Window resizing blocks event processing,
            -- see https://github.com/glfw/glfw/issues/1
            liftIO $ traceS TLInfo $ printf "Window resized: %i x %i" w h
        GLFWEventFramebufferSize {- win -} _ w h ->
            liftIO $ setupViewport w h
        -- GLFWEventMouseButton win bttn st mk -> do
        --     return ()
        -- GLFWEventCursorPos win x y -> do
        --     return ()
        -- GLFWEventScroll win x y -> do
        --     return ()
        _ -> return ()

-- Move through an enumeration, but wrap around when hitting the end
wrapSucc, wrapPred :: (Enum a, Bounded a, Eq a) => a -> a
wrapSucc a | a == maxBound = minBound
           | otherwise     = succ a
wrapPred a | a == minBound = maxBound
           | otherwise     = pred a

draw :: AppIO ()
draw = do
    AppEnv   { .. } <- ask
    AppState { .. } <- get
    -- Clear
    liftIO $ do
        GL.clearColor GL.$= (GL.Color4 1 0 1 1 :: GL.Color4 GL.GLclampf)
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.depthFunc GL.$= Just GL.Lequal
    -- Draw fractal into our frame buffer texture
    void . fillFrameBuffer _aeFB $ \w h fbVec ->
        liftIO $ case _asMode of
            ModeJuliaAnim        -> juliaAnimated w h fbVec False _asCurTick
            ModeJuliaAnimSmooth  -> juliaAnimated w h fbVec True  _asCurTick
            ModeMandelBrot       -> mandelbrot    w h fbVec False
            ModeMandelBrotSmooth -> mandelbrot    w h fbVec True
    -- Render everything quad based
    (liftIO $ GLFW.getFramebufferSize _aeWindow) >>= \(w, h) ->
        void . withQuadRenderBuffer _aeQR w h $ \qb -> do
            -- Draw frame buffer contents
            liftIO $ drawFrameBuffer _aeFB qb
            -- FPS counter and mode display
            ftStr <- updateAndReturnFrameTimes
            liftIO . drawTextWithShadow _aeFontTexture qb 3 (h - 12) $
                printf "Mode %i of %i [-][=]: %s | [S]creenshot | 2x[ESC] Exit\n%s"
                       (fromEnum _asMode + 1 :: Int)
                       (fromEnum (maxBound :: Mode) + 1 :: Int)
                       (show _asMode)
                       ftStr

updateAndReturnFrameTimes :: MonadState AppState m => m String
updateAndReturnFrameTimes = do
    frameTimes <- use $ asFrameTimes.to BS.toList
    curTick    <- use asCurTick
    asFrameTimes %= BS.push_ curTick
    let frameDeltas      = case frameTimes of (x:xs) -> goFD x xs; _ -> []
        goFD prev (x:xs) = (prev - x) : goFD x xs
        goFD _ []        = []
        fdMean           = sum frameDeltas / (fromIntegral $ length frameDeltas)
        fdWorst          = case frameDeltas of [] -> 0; xs -> maximum xs
        fdBest           = case frameDeltas of [] -> 0; xs -> minimum xs
     in return $ printf "%.1fFPS/%.1fms (Worst: %.1f, Best: %.1f)"
                        (1.0 / fdMean ) (fdMean  * 1000)
                        (1.0 / fdWorst)
                        (1.0 / fdBest )

drawTextWithShadow :: GL.TextureObject -> QuadRenderBuffer -> Int -> Int -> String -> IO ()
drawTextWithShadow tex qb x y str = do
    drawText tex qb (x + 1) (y - 1) 0x007F7F7F str
    drawText tex qb  x       y      0x0000FF00 str

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- view aeWindow
    liftIO $ do
        (w, h) <- liftIO $ GLFW.getFramebufferSize window
        setupViewport w h
        GLFW.swapInterval 0
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

