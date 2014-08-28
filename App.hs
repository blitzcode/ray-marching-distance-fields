
{-# LANGUAGE RecordWildCards, TemplateHaskell, LambdaCase, FlexibleContexts #-}

module App ( AppState(..)
           , AppEnv(..)
           , Mode(..)
           , AppT
           , runAppT
           , run
             -- Export to silence warnings
           , aeFontTexture
           , aeQR
           , aeGPUFrac3D
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
import Data.List

import GLFWHelpers
import GLHelpers
import Timing
import Trace
import Font
import FrameBuffer
import Fractal2D
import GPUFractal3D
import QuadRendering
import qualified BoundedSequence as BS

data Mode = ModeMandelBrot
          | ModeMandelBrotSmooth
          | ModeJuliaAnim
          | ModeJuliaAnimSmooth
          | ModeDECornellBoxShader
          | ModeDETestShader
          | ModeMBPower8Shader
          | ModeMBGeneralShader
            deriving (Enum, Eq, Bounded, Show)

data AppState = AppState { _asCurTick          :: !Double
                         , _asLastEscPress     :: !Double
                         , _asFrameTimes       :: BS.BoundedSequence Double
                         , _asMode             :: !Mode
                         , _asFBScale          :: !Float
                         , _asLastShdErr       :: !String
                         }

data AppEnv = AppEnv { _aeWindow           :: GLFW.Window
                     , _aeGLFWEventsQueue  :: TQueue GLFWEvent
                     , _aeFontTexture      :: GL.TextureObject
                     , _aeFB               :: FrameBuffer
                     , _aeQR               :: QuadRenderer
                     , _aeGPUFrac3D        :: GPUFractal3D
                     , _aeShaderModChecker :: IO Bool
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
        GLFWEventKey win k {- sc -} _ ks mk | ks == GLFW.KeyState'Pressed ->
            case k of
                GLFW.Key'Escape -> do
                    lastPress <- use asLastEscPress
                    tick      <- use asCurTick
                    -- Only close when ESC has been pressed twice quickly
                    when (tick - lastPress < 0.5) .
                        liftIO $ GLFW.setWindowShouldClose win True
                    asLastEscPress .= tick
                -- Also clear frame time history on mode / scaling switch
                GLFW.Key'Minus -> asMode %= wrapPred >> asFrameTimes %= BS.clear
                GLFW.Key'Equal -> asMode %= wrapSucc >> asFrameTimes %= BS.clear
                GLFW.Key'L
                    | GLFW.modifierKeysShift mk -> asFBScale %= min 16    . (* 2) >> resize
                    | otherwise                 -> asFBScale %= max 0.125 . (/ 2) >> resize
                GLFW.Key'S     -> view aeFB >>= \fb -> liftIO $ saveFrameBufferToPNG fb .
                                    map (\c -> if c `elem` ['/', '\\', ':', ' '] then '-' else c)
                                      . printf "Screenshot-%s.png" =<< show <$> getZonedTime
                _              -> return ()
        GLFWEventFramebufferSize {- win -} _ {- w -} _ {- h -} _ -> resize
        -- GLFWEventWindowSize {- win -} _ w h -> do
        --     liftIO $ traceS TLInfo $ printf "Window resized: %i x %i" w h
        --     return ()
        -- GLFWEventMouseButton win bttn st mk -> do
        --     return ()
        -- GLFWEventCursorPos win x y -> do
        --     return ()
        -- GLFWEventScroll win x y -> do
        --     return ()
        _ -> return ()

-- Handle changes in window and frame buffer size / scaling
resize :: AppIO ()
resize = do
    scale  <- use asFBScale
    window <- view aeWindow
    fb     <- view aeFB
    liftIO $ do (w, h) <- GLFW.getFramebufferSize window
                setupViewport w h
                resizeFrameBuffer fb
                                  (round $ fromIntegral w * scale)
                                  (round $ fromIntegral h * scale)
    asFrameTimes %= BS.clear

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
    let fillFB             = void . fillFrameBuffer _aeFB
        drawFB             = void . drawIntoFrameBuffer _aeFB
        drawGPUFractal shd w h = drawGPUFractal3D _aeGPUFrac3D shd w h _asCurTick
     in liftIO $ case _asMode of
        ModeJuliaAnim          -> fillFB $ \w h fbVec -> juliaAnimated w h fbVec False _asCurTick
        ModeJuliaAnimSmooth    -> fillFB $ \w h fbVec -> juliaAnimated w h fbVec True  _asCurTick
        ModeMandelBrot         -> fillFB $ \w h fbVec -> mandelbrot    w h fbVec False
        ModeMandelBrotSmooth   -> fillFB $ \w h fbVec -> mandelbrot    w h fbVec True
        ModeDECornellBoxShader -> drawFB $ \w h       -> drawGPUFractal FSDECornellBoxShader w h
        ModeDETestShader       -> drawFB $ \w h       -> drawGPUFractal FSDETestShader       w h
        ModeMBPower8Shader     -> drawFB $ \w h       -> drawGPUFractal FSMBPower8Shader     w h
        ModeMBGeneralShader    -> drawFB $ \w h       -> drawGPUFractal FSMBGeneralShader    w h
    -- Render everything quad based
    (liftIO $ GLFW.getFramebufferSize _aeWindow) >>= \(w, h) ->
        void . withQuadRenderBuffer _aeQR w h $ \qb -> do
            -- Draw frame buffer contents
            liftIO $ drawFrameBuffer _aeFB qb 0 0 (fromIntegral w) (fromIntegral h)
            -- FPS counter and mode display
            liftIO $ drawQuad qb
                              0                (fromIntegral h - 24)
                              (fromIntegral w) (fromIntegral h)
                              2
                              FCBlack
                              (TRBlend 0.5)
                              Nothing
                              QuadUVDefault
            ftStr <- updateAndReturnFrameTimes
            (fbWdh, fbHgt) <- liftIO $ getFrameBufferDim _aeFB
            liftIO . drawTextWithShadow _aeFontTexture qb 3 (h - 12) $
                printf ( "Mode %i/%i [-][=]: %s | [S]creenshot | 2x[ESC] Exit\n" ++
                         "FB Sca[l]e: %fx, Dim %ix%i | %s"
                       )
                       (fromEnum _asMode + 1 :: Int)
                       (fromEnum (maxBound :: Mode) + 1 :: Int)
                       (show _asMode)
                       _asFBScale
                       fbWdh
                       fbHgt
                       ftStr
            -- Display any shader compilation errors from the last reload in a text overlay
            unless (null _asLastShdErr) $
                let wrap    =   concat
                              . intersperse "\n"
                              . map (foldr (\(i, c) str -> if   i > 0 && i `mod` lineWdh == 0
                                                           then c : '\n' : str
                                                           else c : str
                                           ) "" . zip ([0..] :: [Int])
                                    )
                              . filter (/= "\n")
                              . filter (/= "\0") -- No idea why that's in there...
                              . groupBy (\a b -> a /= '\n' && b /= '\n')
                              $ _asLastShdErr
                    lineWdh = (w - 20) `div` 6 - 1
                    errHgt  = (+ 3) . (* 11) . succ . length . filter (== '\n') $ wrap
                    errY    = h `div` 2 + errHgt `div` 2
                 in liftIO $ do drawTextWithShadow _aeFontTexture qb 10 (errY - 12) wrap
                                drawQuad qb
                                         7                      (fromIntegral errY)
                                         (fromIntegral $ w - 7) (fromIntegral $ errY - errHgt)
                                         2
                                         FCBlack
                                         (TRBlend 0.5)
                                         Nothing
                                         QuadUVDefault

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
     in return $ printf "%.2fFPS/%.1fms (Worst: %.2f, Best: %.2f)"
                        (1.0 / fdMean ) (fdMean  * 1000)
                        (1.0 / fdWorst)
                        (1.0 / fdBest )

drawTextWithShadow :: GL.TextureObject -> QuadRenderBuffer -> Int -> Int -> String -> IO ()
drawTextWithShadow tex qb x y str = do
    drawText tex qb (x + 1) (y - 1) 0x00000000 str
    drawText tex qb  x       y      0x0000FF00 str

-- Check if our shader file has been modified on disk and reload shaders if it has been
checkShaderModified :: AppIO ()
checkShaderModified = do
    checker  <- view aeShaderModChecker
    modified <- liftIO checker
    when modified $
        view aeGPUFrac3D >>= liftIO . loadAndCompileShaders >>=
            \case Left err -> do liftIO . traceS TLError $ "Failed to reload shaders:\n" ++ err
                                 asLastShdErr .= err
                  Right s  -> do liftIO . traceS TLInfo $ printf "Reloaded shaders in %.2fs" s
                                 asLastShdErr .= ""

run :: AppIO ()
run = do
    -- Setup OpenGL / GLFW
    window <- view aeWindow
    resize
    liftIO $ GLFW.swapInterval 0
    -- Main loop
    let loop frameIdx = do
          asCurTick <~ liftIO getTick
          tqGLFW <- view aeGLFWEventsQueue
          processAllEvents tqGLFW processGLFWEvent
          checkShaderModified
          -- GLFW / OpenGL
          draw
          liftIO $ {-# SCC swapAndPoll #-} do
              -- GL.flush
              -- GL.finish
              GLFW.swapBuffers window
              GLFW.pollEvents
              traceOnGLError $ Just "main loop"
          -- Drop the first three frame deltas, they are often outliers
          when (frameIdx < 3) $ asFrameTimes %= BS.clear
          -- Done?
          flip unless (loop $ frameIdx + 1) =<< liftIO (GLFW.windowShouldClose window)
     in loop (0 :: Int)

