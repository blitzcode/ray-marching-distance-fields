
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module ShaderRendering ( withShaderRenderer
                       , loadAndCompileShaders
                       , ShaderRenderer
                       , drawShaderTile
                       , FragmentShader(..)
                       , isTileIdxFirstTile
                       , isTileIdxLastTile
                       ) where

import Control.Exception
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Concurrent.Async
import System.Directory
import System.FilePath
import Data.Monoid
import Text.Printf
import qualified Data.ByteString as B
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLR
import qualified Codec.Picture as JP

import Trace
import Timing
import GLHelpers
import GLSLHelpers
import ShaderRenderingVertexShaderSrc
import HDREnvMap
import CornellBox

data ShaderRenderer = ShaderRenderer { srVAO               :: !GL.VertexArrayObject
                                     , srShdFn             :: !FilePath
                                     , srDECornellBoxShd   :: !GL.Program
                                     , srDETestShd         :: !GL.Program
                                     , srMBPower8Shd       :: !GL.Program
                                     , srMBGeneralShd      :: !GL.Program
                                     , srEnvCubeMaps       :: [(String, GL.TextureObject)]
                                     , srCornellBoxGeomTex :: !GL.TextureObject
                                     }

data FragmentShader = FSDECornellBoxShader | FSDETestShader | FSMBPower8Shader | FSMBGeneralShader
                      deriving (Show, Eq, Enum)

tilesX, tilesY, nTiles :: Int
tilesX = 8
tilesY = 8
nTiles = tilesX * tilesY

isTileIdxLastTile :: Int -> Bool
isTileIdxLastTile idx = idx `mod` nTiles == nTiles - 1

isTileIdxFirstTile :: Int -> Bool
isTileIdxFirstTile idx = idx `mod` nTiles == 0

withShaderRenderer :: FilePath -> FilePath -> (ShaderRenderer -> IO a) -> IO a
withShaderRenderer srShdFn reflMapFn f = do
    -- Create, compile and link shaders, load resources
    r <- runExceptT . runResourceT $ do
             srVAO <- genObjectNameResource
             -- Load reflection environment map
             envStart <- liftIO getTick
             reflMap <- either throwError return =<< liftIO (loadHDRImage reflMapFn)
                        -- Build debug environment map
                        -- return buildTestLatLongEnvMap
             -- Build / verify cache of pre-convolved environment maps
             let powers = [1, 8, 64, 512]
                 mkCacheFn pow | pow == 0  = reflMapFn
                               | otherwise = dropExtension reflMapFn ++
                                             "_cache_pow_" ++ show pow ++ ".hdr"
                 powfn = map (\pow -> (pow, mkCacheFn pow)) powers -- Power / cache file nm. pairs
             liftIO $ buildPreConvolvedHDREnvMapCache reflMap powfn
             -- Load pre-convolved environment maps, convert to cube maps and upload to GPU
             --
             -- TODO: Speed up load time by loading and converting environment map texture in a
             --       background thread, display white or a cached scale version in the meantime
             let convertAndAllocCM envMap =
                     snd <$> allocate (latLongHDREnvMapToCubeMap envMap False) GL.deleteObjectName
             envCubeMaps <- forM powfn $ \(pow, fn) -> do
                 envMap <- either throwError return =<< liftIO (loadHDRImage fn)
                 tex    <- convertAndAllocCM envMap
                 return ( "env_cos_" ++ show (round pow :: Int) -- Shader uniform name
                        , tex                                   -- Cube map texture
                        )
             -- Add regular reflection environment map and store in record
             reflCubeMap <- convertAndAllocCM reflMap
             let srEnvCubeMaps = ("env_reflection", reflCubeMap) : envCubeMaps
             envEnd <- liftIO getTick
             -- Create fragment shaders
             [srDECornellBoxShd, srDETestShd, srMBPower8Shd, srMBGeneralShd] <-
                 forM ([0..3] :: [Int]) $ \_ ->
                     snd <$> allocate GL.createProgram GL.deleteObjectName
             -- Cornell box geometry texture
             srCornellBoxGeomTex <-
                snd <$> allocate mkCornellBoxVerticesTex GL.deleteObjectName
             -- Record
             let sr = ShaderRenderer { .. }
             -- Shaders
             shaderTime <- either throwError return
                               =<< liftIO (loadAndCompileShaders sr)
             -- Statistics
             liftIO . traceS TLInfo $ printf
                "withShaderRenderer - Shader time: %.2fs, EnvMap time: %.2fs"
                shaderTime (envEnd - envStart)
             liftIO $ f sr
    either (traceAndThrow . printf "withShaderRenderer - Init failed:\n%s") return r

loadAndCompileShaders :: ShaderRenderer -> IO (Either String Double)
loadAndCompileShaders ShaderRenderer { .. } = runExceptT $ do
    -- Fragment shader is loaded from a file
    shaderStart <- liftIO getTick
    fsSrc <- either (\(e :: IOException) -> throwError $ show e) return
                 =<< (liftIO . try . B.readFile $ srShdFn)
    -- Generate several shader variations through GLSL's pre-processor
    forM_ [ (srDECornellBoxShd, "#define CORNELL_BOX_SCENE"                 )
          , (srDETestShd      , ""                                          )
          , (srMBPower8Shd    , "#define MANDELBULB_SCENE\n#define POWER8\n")
          , (srMBGeneralShd   , "#define MANDELBULB_SCENE\n"                )
          ]
          $ \(shd, defines) ->
                let src = "#version 330 core\n" <> defines <> fsSrc
                 in either throwError return =<<
                        liftIO (compileShaderProgram vsSrcFSQuad src [] shd)
    shaderEnd <- liftIO getTick
    return $ shaderEnd - shaderStart -- Return shader load, compile and link time

buildPreConvolvedHDREnvMapCache :: JP.Image JP.PixelRGBF -> [(Float, FilePath)] -> IO ()
buildPreConvolvedHDREnvMapCache reflMap powfn = do
    -- Check if we have any pre-convolved files missing
    missing <- filterM (fmap not . doesFileExist . snd) powfn
    unless (null missing) $ do
        traceS TLInfo $ printf "Missing %i pre-convolved environment map(s), computing..."
            (length missing)
        -- We compute the pre-convolved versions from a small, downsampled reflection map
        (timeResized, resized) <- timeIt . evaluate . force $ resizeHDRImage reflMap 256
        traceS TLInfo $ printf "Downsampled reflection environment in %.2fs" timeResized
        -- Compute missing convolutions in parallel
        void $ flip mapConcurrently missing $ \(pow, fn) -> do
            (timeConvolved, convolved) <- timeIt . evaluate . force $
                cosineConvolveHDREnvMap resized pow
            traceS TLInfo $ printf "Computed power %.1f in %.2fs wallclock" pow timeConvolved
            (timeWritten, _) <- timeIt $ onException
                (JP.saveRadianceImage fn . JP.ImageRGBF $ convolved)
                (removeFile fn) -- Delete cache image file on error / cancellation
            traceS TLInfo $ printf "Written '%s' in %.2fs" (takeFileName fn) timeWritten

drawShaderTile :: ShaderRenderer -> FragmentShader -> Maybe Int -> Int -> Int -> Double -> IO ()
drawShaderTile ShaderRenderer { .. } shdEnum tileIdx w h time = do
    -- We need a dummy VAO active with all vertex attributes disabled
    GL.bindVertexArrayObject GL.$= Just srVAO
    -- Bind shader
    let shd = case shdEnum of
                  FSDECornellBoxShader -> srDECornellBoxShd
                  FSDETestShader       -> srDETestShd
                  FSMBPower8Shader     -> srMBPower8Shd
                  FSMBGeneralShader    -> srMBGeneralShd
    GL.currentProgram GL.$= Just shd
    -- Only set shader parameters on the first tile, don't want them to change
    -- over the course of a single frame
    when (case tileIdx of Nothing -> True; Just idx -> isTileIdxFirstTile idx) $ do
        -- Setup uniforms
        let uniformFloat nm val =
                GL.get (GL.uniformLocation shd nm) >>= \(GL.UniformLocation loc) ->
                    GLR.glUniform1f loc val
         in do uniformFloat "in_screen_wdh" $ fromIntegral w
               uniformFloat "in_screen_hgt" $ fromIntegral h
               uniformFloat "in_time"       $ realToFrac time
        -- Setup environment cube maps
        forM_ (zip srEnvCubeMaps ([0..] :: [Int])) $ \((uniformName, tex), tuIdx) -> do
            setTextureShader tex GL.TextureCubeMap tuIdx shd uniformName
        -- Cornell box geometry texture
        setTextureShader srCornellBoxGeomTex GL.Texture1D (length srEnvCubeMaps) shd "cornell_geom"
    -- Don't need any VBO etc, the vertex shader will make this a proper quad.
    -- Specify one dummy attribute, as some drivers apparently have an issue
    -- with this otherwise (http://stackoverflow.com/a/8041472/1898360)
    GLR.glVertexAttrib1f 0 0
    -- Optionally draw the full screen quad in tiles to prevent shader timeouts and increase UI
    -- responsibility when we're rendering very complex images or at very high resolution
    let (x0, y0, x1, y1) =
            case tileIdx of
                Nothing   -> (-1, -1, 1, 1)
                Just idx  -> let midx = idx  `mod` nTiles
                                 tx   = midx `mod` tilesX
                                 ty   = midx `div` tilesX
                              in ( (-1 + fromIntegral  tx      / fromIntegral tilesX * 2)
                                 , (-1 + fromIntegral  ty      / fromIntegral tilesY * 2)
                                 , (-1 + fromIntegral (tx + 1) / fromIntegral tilesX * 2)
                                 , (-1 + fromIntegral (ty + 1) / fromIntegral tilesY * 2)
                                 )
     in GL.get (GL.uniformLocation shd "quad") >>= \(GL.UniformLocation loc) ->
            GLR.glUniform4f loc x0 y0 x1 y1
    GL.drawArrays GL.TriangleStrip 0 4

