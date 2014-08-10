
{-# LANGUAGE QuasiQuotes #-}

module GPUFractal3DShaderSource ( vsSrcFSQuad
                                , fsSrcBasic
                                ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import QQPlainText

-- Shaders for ray marching distance estimated fractals
--
-- References:
--
-- http://blog.hvidtfeldts.net/index.php/2011/06/distance-estimated-3d-fractals-part-i/
-- http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
-- http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf
-- https://www.shadertoy.com/view/MdfGRr

vsSrcFSQuad, fsSrcBasic :: B.ByteString

vsSrcFSQuad = TE.encodeUtf8 . T.pack $ [plaintext|

#version 330 core

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

fsSrcBasic = TE.encodeUtf8 . T.pack $ [plaintext|

#version 330 core
in vec2 fs_uv;
out vec4 frag_color;

float de_sphere(vec3 pos)
{
    return max(0.0, length(pos) - 0.3);
}

float de_torus(vec3 pos)
{
    float torus_size = 0.8;
    float torus_r = 0.2;
    vec2 q = vec2(length(pos.xy) - torus_size, pos.z);
    return length(q) - torus_r;
}

float de_rounded_box(vec3 pos)
{
    vec3 box = vec3(0.1, 0.1, 0.85);
    float r = 0.05;
    return length(max(abs(pos) - box, 0.0)) - r;
}

float smin(float a, float b, float k)
{
    float res = exp(-k * a) + exp(-k * b);
    return -log(res) / k;
}

float distance_estimator(vec3 pos)
{
    return smin(de_rounded_box(pos), smin(de_sphere(pos), de_torus(pos), 32), 32);
}

vec3 normal(vec3 pos)
{
    const float eps = 0.001;
    const vec3 epsX = vec3(eps, 0.0, 0.0);
    const vec3 epsY = vec3(0.0, eps, 0.0);
    const vec3 epsZ = vec3(0.0, 0.0, eps);
    return normalize(vec3(distance_estimator(pos + epsX) - distance_estimator(pos - epsX),
                          distance_estimator(pos + epsY) - distance_estimator(pos - epsY),
                          distance_estimator(pos + epsZ) - distance_estimator(pos - epsZ)));
}

vec3 ray_march(vec3 origin, vec3 dir)
{
    const int MAX_STEPS = 64;
    const float MIN_DIST = 0.001;

    float dist_sum = 0.0, dist;
    int steps = 0;
    for (steps=0; steps < MAX_STEPS; steps++)
    {
        vec3 pos = origin + dist_sum * dir;
        dist = distance_estimator(pos);
        dist_sum += dist;
        if (dist < MIN_DIST)
            break;
    }
    return vec3 ( dist_sum
                , dist
                , 1.0 - float(steps) / float(MAX_STEPS)
                );
}

void main()
{
    vec3 origin = vec3(fs_uv, 1.1);
    vec3 dir    = vec3(0.0, 0.0, -1.0);

    vec3  r             = ray_march(origin, dir);
    float t             = r.x;
    float last_dist     = r.y;
    float step_gradient = r.z;

    if (last_dist < 0.01)
    {
        vec3 isec_pos = origin + dir * t;
        vec3 isec_n = normal(isec_pos - dir * 0.01);
        frag_color = vec4(isec_n, 1);
    }
    else
        frag_color = vec4(0, 0, 0, 1);

    /*
    // Debug: Fill entire clip space interval with a bordered rectangle
    if (fs_uv.x >= -1.0 && fs_uv.x <= 1.0 && fs_uv.y >= -1.0 && fs_uv.y <= 1.0)
        if (fs_uv.x >= -0.99 && fs_uv.x <= 0.99 && fs_uv.y >= -0.99 && fs_uv.y <= 0.99)
            frag_color = vec4(1, 1, 1, 1);
        else
            frag_color = vec4(1, 0, 0, 1);
    else
        frag_color = vec4(0, 0, 0, 1);
    */
}

|]

