
module GLImmediate ( color3f
                   , color4f
                   , vertex2f
                   , vertex3f
                   , texCoord2f
                   ) where

import qualified Graphics.Rendering.OpenGL as GL

-- Immediate mode OpenGL helper functions

{-# INLINEABLE color3f #-}
color3f :: Float -> Float -> Float -> IO ()
color3f r g b = GL.color $ GL.Color3 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b)

{-# INLINEABLE color4f #-}
color4f :: Float -> Float -> Float -> Float -> IO ()
color4f r g b a = GL.color $
    GL.Color4 (realToFrac r :: GL.GLfloat) (realToFrac g) (realToFrac b) (realToFrac a)

{-# INLINEABLE vertex2f #-}
vertex2f :: Float -> Float -> IO ()
vertex2f x y = GL.vertex $ GL.Vertex2 (realToFrac x :: GL.GLfloat) (realToFrac y)

{-# INLINEABLE vertex3f #-}
vertex3f :: Float -> Float -> Float -> IO ()
vertex3f x y z = GL.vertex $
    GL.Vertex3 (realToFrac x :: GL.GLfloat) (realToFrac y) (realToFrac z)

{-# INLINEABLE texCoord2f #-}
texCoord2f :: Float -> Float -> IO ()
texCoord2f u v = GL.texCoord $ GL.TexCoord2 (realToFrac u :: GL.GLfloat) (realToFrac v)

