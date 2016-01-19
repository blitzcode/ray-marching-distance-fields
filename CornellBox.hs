
{-# LANGUAGE OverloadedLists #-}

module CornellBox ( mkCornellBoxVerticesTex
                  , cornellBox
                  ) where

import Control.Exception
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Marshal.Array
import Linear

import GLHelpers

-- Build a 1D OpenGL floating point texture containing the vertices of
-- the triangulated Cornell Box geometry, scaled and centered to [-1, 1]
--
-- http://www.graphics.cornell.edu/online/box/data.html

mkCornellBoxVerticesTex :: IO GL.TextureObject
mkCornellBoxVerticesTex =
    bracketOnError GL.genObjectName GL.deleteObjectName $ \tex -> do
        GL.textureBinding GL.Texture1D GL.$= Just tex
        setTextureFiltering GL.Texture1D TFNone
        let numQuad = V.length cornellBox `div` 4
            numTri  = numQuad * 2
            numVtx  = numTri * 3
            toUnit  = 559.2 / 2
            scale   = 1 / (sqrt (2 * 2 + 2 * 2 + 2 * 2) / 2) * 0.99
            vtx     = flip concatMap ([0..numQuad - 1] :: [Int]) $ \quadIdx ->
                          [ ((cornellBox V.! (quadIdx * 4 + 0)) / toUnit - 1) ^* scale
                          , ((cornellBox V.! (quadIdx * 4 + 1)) / toUnit - 1) ^* scale
                          , ((cornellBox V.! (quadIdx * 4 + 3)) / toUnit - 1) ^* scale
                          , ((cornellBox V.! (quadIdx * 4 + 3)) / toUnit - 1) ^* scale
                          , ((cornellBox V.! (quadIdx * 4 + 1)) / toUnit - 1) ^* scale
                          , ((cornellBox V.! (quadIdx * 4 + 2)) / toUnit - 1) ^* scale
                          ]
         in withArray vtx $ GL.texImage1D GL.Texture1D
                                          GL.NoProxy
                                          0
                                          GL.RGB32F
                                          (GL.TextureSize1D $ fromIntegral numVtx)
                                          0
                                          . GL.PixelData GL.RGB GL.Float
        return tex

cornellBox :: V.Vector (V3 Float)
cornellBox =
    [ -- Floor (White)
      V3 552.8   0.0       0.0
    , V3 0.0     0.0       0.0
    , V3 0.0     0.0       559.2
    , V3 549.6   0.0       559.2

      -- Ceiling (White)
    , V3 556.0   548.8     0.0
    , V3 556.0   548.8     559.2
    , V3 0.0     548.8     559.2
    , V3 0.0     548.8     0.0

      -- Back Wall (White)
    , V3 549.6   0.0       559.2
    , V3 0.0     0.0       559.2
    , V3 0.0     548.8     559.2
    , V3 556.0   548.8     559.2

      -- Right Wall (Green)
    , V3 0.0     0.0       559.2
    , V3 0.0     0.0       0.0
    , V3 0.0     548.8     0.0
    , V3 0.0     548.8     559.2

      -- Left Wall (Red)
    , V3 552.8   0.0       0.0
    , V3 549.6   0.0       559.2
    , V3 556.0   548.8     559.2
    , V3 556.0   548.8     0.0

      -- Light (Small offset to avoid surface acne)
    , V3 343.0   (548.8 - 0.1) 227.0
    , V3 343.0   (548.8 - 0.1) 332.0
    , V3 213.0   (548.8 - 0.1) 332.0
    , V3 213.0   (548.8 - 0.1) 227.0

      -- Short Block (White)
    , V3 130.0   165.0     65.0
    , V3 82.0    165.0     225.0
    , V3 240.0   165.0     272.0
    , V3 290.0   165.0     114.0
    , V3 290.0   0.0       114.0
    , V3 290.0   165.0     114.0
    , V3 240.0   165.0     272.0
    , V3 240.0   0.0       272.0
    , V3 130.0   0.0       65.0
    , V3 130.0   165.0     65.0
    , V3 290.0   165.0     114.0
    , V3 290.0   0.0       114.0
    , V3 82.0    0.0       225.0
    , V3 82.0    165.0     225.0
    , V3 130.0   165.0     65.0
    , V3 130.0   0.0       65.0
    , V3 240.0   0.0       272.0
    , V3 240.0   165.0     272.0
    , V3 82.0    165.0     225.0
    , V3 82.0    0.0       225.0

      -- Tall Block (White)
    , V3 423.0   330.0     247.0
    , V3 265.0   330.0     296.0
    , V3 314.0   330.0     456.0
    , V3 472.0   330.0     406.0
    , V3 423.0   0.0       247.0
    , V3 423.0   330.0     247.0
    , V3 472.0   330.0     406.0
    , V3 472.0   0.0       406.0
    , V3 472.0   0.0       406.0
    , V3 472.0   330.0     406.0
    , V3 314.0   330.0     456.0
    , V3 314.0   0.0       456.0
    , V3 314.0   0.0       456.0
    , V3 314.0   330.0     456.0
    , V3 265.0   330.0     296.0
    , V3 265.0   0.0       296.0
    , V3 265.0   0.0       296.0
    , V3 265.0   330.0     296.0
    , V3 423.0   330.0     247.0
    , V3 423.0   0.0       247.0
    ]

