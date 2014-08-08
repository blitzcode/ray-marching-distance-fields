
{-# LANGUAGE QuasiQuotes #-}

module GPUFractal3DShaderSource ( vsSrcFSQuad
                                , fsSrcBasic
                                ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

vsSrcFSQuad, fsSrcBasic :: B.ByteString

vsSrcFSQuad = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
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

fsSrcBasic = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                out vec4 frag_color;
                void main()
                {
                   frag_color = vec4(1, 0, 0, 1);
                }
    |]

