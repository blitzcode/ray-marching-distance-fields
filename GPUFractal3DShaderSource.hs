
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

-- TODO: Need to add some form of near plane clipping
-- TODO: Have a maxT parameter to abort marching
-- TODO: Implement perspective camera
-- TODO: Move transformations into vertex shader, like here:
--       http://blog.hvidtfeldts.net/index.php/2014/01/combining-ray-tracing-and-polygons/
-- TODO: Better AO based on distance estimation along the surface normal
-- TODO: Coloring with orbit traps
-- TODO: IBL, draw Env. as background, analytically project normal into SH for lookup
-- TODO: Encode HDR Env Maps to SH, store as raw numbers in shader
-- TODO: Maybe generate variations of the shader by running it through cpphs?
-- TODO: Separate shader entry points for distance field tests, arbitrary power code and
--       transcendental free power 8 code, SoftLamFakeAO and IBL
-- TODO: Add support for tiled rendering, preventing long stalls and shader timeouts
-- TODO: Make upscaled rendering the default, key to switch to tiled high quality rendering
-- TODO: See if we can maybe integrate AntTweakBar or similar
-- TODO: Mouse control for orbiting camera
-- TODO: Adjust ray marching MIN_DIST and FD normal epsilon based screen projection, like in
--       https://www.shadertoy.com/view/MdfGRr

vsSrcFSQuad, fsSrcBasic :: B.ByteString

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

fsSrcBasic = TE.encodeUtf8 . T.pack $ [plaintext|

#version 330 core

uniform float in_time;
uniform float in_screen_wdh;
uniform float in_screen_hgt;

out vec4 frag_color;

// http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
float de_sphere(vec3 pos, float r)
{
    return max(0.0, length(pos) - r);
}
float de_torus(vec3 pos, float torus_size, float torus_r)
{
    vec2 q = vec2(length(pos.xy) - torus_size, pos.z);
    return length(q) - torus_r;
}
float de_rounded_box(vec3 pos, vec3 box, float r)
{
    return length(max(abs(pos) - box, 0.0)) - r;
}

#define POWER8

float de_mandelbulb(vec3 pos)
{
    float pow_offs = mod(in_time, 10);
    if (pow_offs > 5)
        pow_offs = 10 - pow_offs;
    float power            =
#ifdef POWER8
        8;
#else
        pow_offs + 2;
#endif
    const float bailout    = 4;
    const int   iterations = 100;

    vec3  w  = pos;
    float dr = 1.0;
    float r  = 0.0;
    for (int i=0; i<iterations; i++)
    {
#ifdef POWER8
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
#else
        r = length(w);
        if (r > bailout)
            break;

        // Convert to polar coordinates
        // float theta = acos(w.z / r);
        // float phi   = atan(w.y, w.x);
        float theta = acos(w.y / r);
        float phi   = atan(w.x, w.z);
        dr          = pow(r, power - 1.0) * power * dr + 1.0;

        // Scale and rotate the point
        float zr = pow(r, power);
        theta    = theta * power;
        phi      = phi * power;

        // Convert back to cartesian coordinates
        // w = zr * vec3(sin(theta) * cos(phi), sin(phi) * sin(theta), cos(theta));
        w = zr * vec3(sin(phi) * sin(theta), cos(theta), sin(theta) * cos(phi));
        w += pos;
#endif
    }

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
#if 1
    return de_mandelbulb(pos);
#else
    return smin(de_rounded_box(pos, vec3(0.05, 0.85, 0.05), 0.05),
             smin(de_rounded_box(pos, vec3(0.1, 0.1, 0.85), 0.05),
               smin(de_sphere(pos, 0.3),
                 de_torus(pos, 0.8, 0.2),
                   32), 32), 64);
#endif
}

vec3 normal(vec3 pos)
{
    // Central difference based normal
    const float eps = 0.000001;
    const vec3 epsX = vec3(eps, 0.0, 0.0);
    const vec3 epsY = vec3(0.0, eps, 0.0);
    const vec3 epsZ = vec3(0.0, 0.0, eps);
    return normalize(vec3(distance_estimator(pos + epsX) - distance_estimator(pos - epsX),
                          distance_estimator(pos + epsY) - distance_estimator(pos - epsY),
                          distance_estimator(pos + epsZ) - distance_estimator(pos - epsZ)));
}

bool ray_sphere( vec3 origin
               , vec3 dir
               , vec3 spherePos
               , float sphereR
               , out float tmin
               , out float tmax
               )
{
    vec3 rs  = spherePos - origin;
    float t  = dot(dir, rs);
    float a  = dot(rs, rs) - t * t;
    float r2 = sphereR * sphereR;
    if (a > r2)
        return false;
    float h = sqrt(r2 - a);
    tmin = t - h;
    tmax = t + h;
    return true;
}

void ray_march( vec3 origin
              , vec3 dir
              , out float t             // T along the ray
              , out float dist          // Last distance (to see if we got close to anything)
              , out float step_gradient // Step based gradient (for cheap fake AO)
              )
{
    // Ray march till we come close enough to a surface or exceed the iteration count

    const int   MAX_STEPS = 64;
    const float MIN_DIST  = 0.001;

    // First intersect with a bounding sphere. Helps quickly reject rays which can't
    // possibly intersect with the scene and helps by bringing our starting point closer
    // to the surface (DEs get very imprecise when we're starting to far away)
    float tspheremin, tspheremax;
    if (!ray_sphere(origin, dir, vec3(0,0,0), 1.25, tspheremin, tspheremax))
        return;

    t = tspheremin;
    int steps = 0;
    for (steps=0; steps<MAX_STEPS; steps++)
    {
        vec3 pos = origin + t * dir;
        dist = distance_estimator(pos);
        t += dist;
        if (dist < MIN_DIST/* || dist > tspheremax*/)
            break;
    }

    step_gradient = 1.0 - float(steps) / float(MAX_STEPS);
}

vec3 soft_lam(vec3 n, vec3 light, vec3 surface_col)
{
    vec3  warm_col  = vec3(0.9 , 0.9 , 0.7);
    vec3  cool_col  = vec3(0.07, 0.07, 0.1);
    float diff_warm = 0.25;
    float diff_cool = 0.15;

    float ndotl     = (dot(light, n) + 1.0) * 0.5;

    vec3  kcool     = min((cool_col + diff_cool) * surface_col, 1.0);
    vec3  kwarm     = min((warm_col + diff_warm) * surface_col, 1.0);
    vec3  kfinal    = mix(kcool, kwarm, ndotl);

    return min(kfinal, 1.0);
}

// NDC to camera space
mat4x4 unproj_ortho(float width, float height)
{
    return mat4x4(width / 2.0, 0.0         , 0.0, 0.0,
                  0.0        , height / 2.0, 0.0, 0.0,
                  0.0        , 0.0         , 1.0, 0.0,
                  0.0        , 0.0         , 0.0, 1.0);
}

mat4x4 lookat(vec3 eye, vec3 focus, vec3 up)
{
    vec3 zaxis = normalize(focus - eye);
    vec3 xaxis = normalize(cross(zaxis, up));
    vec3 yaxis = cross(xaxis, zaxis);
    return mat4x4(xaxis.x, xaxis.y, xaxis.z, 0.0,
                  yaxis.x, yaxis.y, yaxis.z, 0.0,
                  zaxis.x, zaxis.y, zaxis.z, 0.0,
                  eye.x  , eye.y  , eye.z  , 1.0);
}

void main()
{
    // Convert fragment coordinates to NDC [-1, 1]
    vec2 ndc = vec2(gl_FragCoord.x / in_screen_wdh, gl_FragCoord.y / in_screen_hgt) * 2.0 - 1.0;

    // Orthographic projection. Frame [-1, 1] on X, center interval on Y while keeping aspect
    float aspect = in_screen_wdh / in_screen_hgt;
    float width  = 2;
    float height = width / aspect;
    mat4x4 unproj = unproj_ortho(width, height);

    // Orbit camera
    vec3 cam_pos;
    cam_pos.x = sin(in_time / 3.0) * 2;
    cam_pos.z = cos(in_time / 3.0) * 2;
    cam_pos.y = cos(in_time / 4.0) * 2;

    // Camera transform. Look at center, orbit around it
    mat4x4 camera = lookat(cam_pos, vec3(0,0,0), vec3(0,1,0));

    // Transform ray
    vec3 origin = (camera * unproj * vec4(ndc, 0.0, 1.0)     ).xyz;
    vec3 dir    = (camera *          vec4(0.0, 0.0, 1.0, 0.0)).xyz;

    // Ray march
    float t, last_dist, step_gradient;
    ray_march(origin, dir, t, last_dist, step_gradient);

    if (last_dist < 0.1)
    {
        // Compute intersection
        vec3 isec_pos = origin + dir * t;

        // Step back from the surface a bit before computing the normal
        vec3 isec_n = normal(isec_pos - dir * 0.00001);

        // Shading
        //vec3 color = vec3(((isec_n + 1) * 0.5) * pow(step_gradient, 2));
        /*vec3 color = ( vec3(max(0, dot(isec_n, normalize(vec3(1, 1, 1))))) * vec3(1,0.75,0.5) +
                       vec3(max(0, dot(isec_n, normalize(vec3(-1, -1, -1))))) * vec3(0.75,1.0,1.0)
                     ) * pow(step_gradient, 3);*/
        vec3 color = soft_lam(isec_n, normalize(vec3(1, 1, 1)), vec3(pow(step_gradient, 3)));

        // Gamma correct and output
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

