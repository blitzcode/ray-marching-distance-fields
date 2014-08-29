
{-# LANGUAGE QuasiQuotes #-}

module ShaderRenderingVertexShaderSrc ( vsSrcFSQuad
                                      ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

vsSrcFSQuad :: B.ByteString
vsSrcFSQuad = TE.encodeUtf8 . T.pack $ [plaintext|

#version 330 core

uniform vec4 quad = vec4(-1.0, -1.0, 1.0, 1.0);

vec2 quad_vtx[4] = vec2[4] ( vec2(quad.x, quad.y)
                           , vec2(quad.z, quad.y)
                           , vec2(quad.x, quad.w)
                           , vec2(quad.z, quad.w)
                           );
void main()
{
    gl_Position = vec4(quad_vtx[gl_VertexID], 0.0, 1.0);
}

|]

