
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
-- --------------------
-- References & Sources
-- --------------------
--
-- http://blog.hvidtfeldts.net/index.php/2011/06/distance-estimated-3d-fractals-part-i/
--
-- http://blog.hvidtfeldts.net/index.php/2011/08/
--     distance-estimated-3d-fractals-ii-lighting-and-coloring/
--
-- http://blog.hvidtfeldts.net/index.php/2011/09/distance-estimated-3d-fractals-iv-the-holy-grail/
--
-- http://blog.hvidtfeldts.net/index.php/2011/09/
--     distance-estimated-3d-fractals-v-the-mandelbulb-different-de-approximations/
--
-- http://blog.hvidtfeldts.net/index.php/2012/05/distance-estimated-3d-fractals-part-viii-epilogue/
--
-- http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
--
-- http://www.iquilezles.org/www/articles/mandelbulb/mandelbulb.htm
--
-- http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf
--
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
uniform float in_time;
out vec4 frag_color;

// http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
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

float de_mandelbulb(vec3 pos)
{
    // http://blog.hvidtfeldts.net/index.php/2011/09/
    // distance-estimated-3d-fractals-v-the-mandelbulb-different-de-approximations/

    float power            = 8;//mod(in_time, 5) + 2;
    const float bailout    = 4;
    const int   iterations = 150;

    vec3  w  = pos;
    float dr = 1.0;
    float r  = 0.0;
    for (int i=0; i<iterations; i++)
    {
        r = length(w);
        if (r > bailout)
            break;

        dr = pow(r, power - 1.0) * power * dr + 1.0;

        float x = w.x; float x2 = x*x; float x4 = x2*x2;
        float y = w.y; float y2 = y*y; float y4 = y2*y2;
        float z = w.z; float z2 = z*z; float z4 = z2*z2;

        float k3 = x2 + z2;
        float k2 = inversesqrt( k3*k3*k3*k3*k3*k3*k3 );
        float k1 = x4 + y4 + z4 - 6.0*y2*z2 - 6.0*x2*y2 + 2.0*z2*x2;
        float k4 = x2 - y2 + z2;

        w.x =  64.0*x*y*z*(x2-z2)*k4*(x4-6.0*x2*z2+z4)*k1*k2;
        w.y = -16.0*y2*k3*k4*k4 + k1*k1;
        w.z = -8.0*y*k4*(x4*x4 - 28.0*x4*x2*z2 + 70.0*x4*z4 - 28.0*x2*z2*z4 + z4*z4)*k1*k2;

        w += pos;
    }
    /*
    {
        r = length(z);
        if (r > bailout)
            break;

        // Convert to polar coordinates
        //float theta = acos(z.z / r);
        //float phi   = atan(z.y, z.x);
        float theta = acos(z.y / r);
        float phi   = atan(z.x, z.z);
        dr          = pow(r, power - 1.0) * power * dr + 1.0;

        // Scale and rotate the point
        float zr = pow(r, power);
        theta    = theta * power;
        phi      = phi * power;

        // Convert back to cartesian coordinates
        //z = zr * vec3(sin(theta) * cos(phi), sin(phi) * sin(theta), cos(theta));
        z = zr * vec3(sin(phi) * sin(theta), cos(theta), sin(theta) * cos(phi));
        z += pos;
    }
    */

    return 0.5 * log(r) * r / dr;
}

float smin(float a, float b, float k)
{
    // http://iquilezles.org/www/articles/smin/smin.htm
    float res = exp(-k * a) + exp(-k * b);
    return -log(res) / k;
}

float distance_estimator(vec3 pos)
{
    // return smin(de_rounded_box(pos), smin(de_sphere(pos), de_torus(pos), 32), 32);
    return de_mandelbulb(pos);
}

vec3 normal(vec3 pos)
{
    // Central difference based normal
    const float eps = 0.00001;
    const vec3 epsX = vec3(eps, 0.0, 0.0);
    const vec3 epsY = vec3(0.0, eps, 0.0);
    const vec3 epsZ = vec3(0.0, 0.0, eps);
    return normalize(vec3(distance_estimator(pos + epsX) - distance_estimator(pos - epsX),
                          distance_estimator(pos + epsY) - distance_estimator(pos - epsY),
                          distance_estimator(pos + epsZ) - distance_estimator(pos - epsZ)));
}

vec3 ray_march(vec3 origin, vec3 dir)
{
    // Ray march till we come close enough to a surface or exceed the iteration count

    const int MAX_STEPS = 64;
    const float MIN_DIST = 0.001;

    float dist_sum = 0.0, dist;
    int steps = 0;
    for (steps=0; steps<MAX_STEPS; steps++)
    {
        vec3 pos = origin + dist_sum * dir;
        dist = distance_estimator(pos);
        dist_sum += dist;
        if (dist < MIN_DIST)
            break;
    }
    return vec3
        ( dist_sum                              // T along the ray
        , dist                                  // Last distance (to see if we got close to anything)
        , 1.0 - float(steps) / float(MAX_STEPS) // Step based gradient (for cheap fake AO)
        );
}

void main()
{
    // Transform ray
    vec3 origin = vec3(fs_uv, 1.1);
    vec3 dir    = vec3(0.0, 0.0, -1.0);

    // Ray march
    vec3  r             = ray_march(origin, dir);
    float t             = r.x;
    float last_dist     = r.y;
    float step_gradient = r.z;

    if (last_dist < 0.01)
    {
        // Compute intersection and normal
        vec3 isec_pos = origin + dir * t;
        vec3 isec_n   = normal(isec_pos - dir * 0.0001);

        // Gamma correct and output
        //vec3 color = vec3(((isec_n + 1) * 0.5) * pow(step_gradient, 2));
        vec3 color = ( vec3(max(0, dot(isec_n, normalize(vec3(1, 1, 1))))) * vec3(1,0.75,0.5) +
                       vec3(max(0, dot(isec_n, normalize(vec3(-1, -1, -1))))) * vec3(0.75,1.0,1.0)
                     ) * pow(step_gradient, 3);
        vec3 gamma = pow(color, vec3(1.0 / 2.2));
        frag_color = vec4(gamma, 1);
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

