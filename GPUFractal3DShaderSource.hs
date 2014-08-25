
{-# LANGUAGE QuasiQuotes #-}

module GPUFractal3DShaderSource ( vsSrcFSQuad
                                ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

vsSrcFSQuad :: B.ByteString
vsSrcFSQuad = TE.encodeUtf8 . T.pack $ [plaintext|

#version 330 core

const vec2 quad_vtx[4] = vec2[4] ( vec2(-1.0, -1.0)
                                 , vec2( 1.0, -1.0)
                                 , vec2(-1.0,  1.0)
                                 , vec2( 1.0,  1.0)
                                 );
void main()
{
    gl_Position = vec4(quad_vtx[gl_VertexID], 0.0, 1.0);
}

|]

