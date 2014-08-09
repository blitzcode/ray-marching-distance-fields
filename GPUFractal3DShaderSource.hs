
{-# LANGUAGE QuasiQuotes #-}

module GPUFractal3DShaderSource ( vsSrcFSQuad
                                , fsSrcBasic
                                ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

-- Shaders based on this eight part series on distance estimated 3D fractals
-- http://blog.hvidtfeldts.net/index.php/2011/06/distance-estimated-3d-fractals-part-i/

vsSrcFSQuad, fsSrcBasic :: B.ByteString

vsSrcFSQuad = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                uniform float in_aspect;
                out vec2 fs_uv;
                const vec2 quad_vtx[4] = vec2[4] ( vec2(-1.0, -1.0)
                                                 , vec2( 1.0, -1.0)
                                                 , vec2(-1.0,  1.0)
                                                 , vec2( 1.0,  1.0)
                                                 );
                float view_height = 1.0 / in_aspect;
                float scale = 1.0f;
                vec2 quad_uv[4] = vec2[4] ( vec2(-1.0 * scale, -view_height * scale)
                                          , vec2( 1.0 * scale, -view_height * scale)
                                          , vec2(-1.0 * scale,  view_height * scale)
                                          , vec2( 1.0 * scale,  view_height * scale)
                                          );
                void main()
                {
                    gl_Position = vec4(quad_vtx[gl_VertexID], 0.0, 1.0);
                    fs_uv       = quad_uv[gl_VertexID];
                }
    |]

fsSrcBasic = TE.encodeUtf8 . T.pack $
    [plaintext| #version 330 core
                in vec2 fs_uv;
                out vec4 frag_color;
                void main()
                {
                   //frag_color = vec4(fs_uv, 0, 1);
                   if (fs_uv.x >= -1.0 && fs_uv.x <= 1.0 && fs_uv.y >= -1.0 && fs_uv.y <= 1.0)
                       if (fs_uv.x >= -0.99 && fs_uv.x <= 0.99 && fs_uv.y >= -0.99 && fs_uv.y <= 0.99)
                           frag_color = vec4(1, 1, 1, 1);
                       else
                           frag_color = vec4(1, 0, 0, 1);
                   else
                       frag_color = vec4(0, 0, 0, 1);
                }
    |]

