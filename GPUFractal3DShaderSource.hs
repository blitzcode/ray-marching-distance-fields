
{-# LANGUAGE QuasiQuotes #-}

module GPUFractal3DShaderSource ( vsSrcFSQuad
                                , fsSrcFractal
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
-- TODO: Have specialized versions of all the integer powers, i.e.
--       http://www.fractalforums.com/index.php?action=dlattach;topic=742.0;attach=429;image
--       http://en.wikipedia.org/wiki/Mandelbulb
-- TODO: Move transformations into vertex shader, like here:
--       http://blog.hvidtfeldts.net/index.php/2014/01/combining-ray-tracing-and-polygons/
-- TODO: IBL, draw Env. as background, analytically project normal into SH for lookup
-- TODO: Encode HDR Env Maps to SH, store as raw numbers in shader
-- TODO: Add support for tiled rendering, preventing long stalls and shader timeouts
-- TODO: Make upscaled rendering the default, key to switch to tiled high quality rendering
-- TODO: See if we can maybe integrate AntTweakBar or similar
-- TODO: Mouse control for orbiting camera
-- TODO: Adjust ray marching MIN_DIST, FD normal epsilon and ray step back
--       based screen projection, like in https://www.shadertoy.com/view/MdfGRr
-- TODO: Understand and try out some of the other DE methods from
--       http://blog.hvidtfeldts.net/index.php/2011/09/
--           distance-estimated-3d-fractals-v-the-mandelbulb-different-de-approximations/
-- TODO: Implement some more BRDFs besides Lambert
-- TODO: Collect epsilons and settings into one place
-- TODO: Could try implementing SSS based on the distance_ao() function
-- TODO: Re-use length(w) term in de_mandelbulb() iteration loop for cartesian_to_spherical()
--       in triplex_pow()
-- TODO: Maybe add some post-process effects like bloom or some fake motion-blur / DoF
-- TODO: Consider 3D texture cache with DE value in each cell, speeding up ray marching
--       till we get close to the surface. Might not help that much as the most expensive
--       DE invocations are the ones close to the surface
-- TODO: Consider a hierarchical Z like setup where we first ray march 4x4 pixel blocks
--       till we get close to the surface and then start off there at pixel resolution
--       Also see
--       http://www.fractalforums.com/mandelbulb-implementation/major-raymarching-optimization/
-- TODO: Iteration count 100 is probably excessively high, find a better trade-off
-- TODO: Check out
--       http://www.fractalforums.com/mandelbulb-implementation/realtime-renderingoptimisations/
-- TODO: See if we can make a cycle detection optimization like for the 2D Mandelbrot

vsSrcFSQuad, fsSrcFractal :: B.ByteString

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

fsSrcFractal = TE.encodeUtf8 . T.pack $ [plaintext|

// '#version 330 core' defined externally when we generate variations of this shader

uniform float in_time;
uniform float in_screen_wdh;
uniform float in_screen_hgt;

uniform samplerCube env;

out vec4 frag_color;

// http://iquilezles.org/www/articles/distfunctions/distfunctions.htm
float de_sphere(vec3 pos, float r)
{
    return length(pos) - r;
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
float de_cone(vec3 pos, vec2 c)
{
    // c must be normalized
    float q = length(pos.xz);
    return dot(c, vec2(q, pos.y));
}

// http://en.wikipedia.org/wiki/Spherical_coordinate_system
void cartesian_to_spherical(vec3 p, out float r, out float theta, out float phi)
{
    r     = length(p);
    theta = acos(p.z / r);
    phi   = atan(p.y, p.x);
}
vec3 spherical_to_cartesian(float r, float theta, float phi)
{
    return r * vec3(sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta));
}

vec3 triplex_pow(vec3 w, float power)
{
    // General pow() for our triplex numbers
    //
    // http://blog.hvidtfeldts.net/index.php/2011/09/
    //     distance-estimated-3d-fractals-iv-the-holy-grail/
    //
    // http://blog.hvidtfeldts.net/index.php/2011/09/
    //     distance-estimated-3d-fractals-v-the-mandelbulb-different-de-approximations/

    float r, theta, phi;
    cartesian_to_spherical(w, r, theta, phi);

    // Scale and rotate the point
    float zr = pow(r, power);
    theta    = theta * power;
    phi      = phi * power;

    return spherical_to_cartesian(zr, theta, phi);
}

vec3 triplex_pow8(vec3 w)
{
    // Optimized pow(x, 8) for our triplex numbers (special case without transcendentals)
    //
    // http://www.iquilezles.org/www/articles/mandelbulb/mandelbulb.htm
    //
    // (modified so the Mandelbulb has the same orientation as the general triplex_pow() one)

    float x = w.x; float x2 = x*x; float x4 = x2*x2;
    float y = w.y; float y2 = y*y; float y4 = y2*y2;
    float z = w.z; float z2 = z*z; float z4 = z2*z2;

    float k3 = y2 + x2;
    float k2 = inversesqrt( k3*k3*k3*k3*k3*k3*k3 );
    float k1 = y4 + z4 + x4 - 6.0*z2*x2 - 6.0*y2*z2 + 2.0*x2*y2;
    float k4 = y2 - z2 + x2;

    return vec3( -8.0*z*k4*(y4*y4 - 28.0*y4*y2*x2 + 70.0*y4*x4 - 28.0*y2*x2*x4 + x4*x4)*k1*k2
               , 64.0*y*z*x*(y2-x2)*k4*(y4-6.0*y2*x2+x4)*k1*k2
               , -16.0*z2*k3*k4*k4 + k1*k1
               );
}

float de_mandelbulb(vec3 pos)
{
#ifdef POWER8
    float power = 8;
#else
    // Animate power
    float pow_offs = mod(in_time / 2, 9);
    if (pow_offs > 4.5)
        pow_offs = 9 - pow_offs;
    float power = pow_offs + 2;
#endif
    const float bailout    = 4;
    const int   iterations = 25;

    // Swap some axis so our Mandelbulb is upright instead of lying on the side
    pos = pos.zxy;

    // Iterate. This is pretty much what we'd do for a Mandelbrot set, except that instead of
    // complex numbers we have triplex numbers with a special power operation that rotates
    // and scales in spherical coordinates
    vec3  w  = pos;
    float dr = 1.0;
    float r  = 0.0;
    // vec3 trap = abs(w);
    for (int i=0; i<iterations; i++)
    {
        r = length(w);
        if (r > bailout)
            break;
#ifdef POWER8
        w = triplex_pow8(w);
#else
        w = triplex_pow(w, power);
#endif
        w += pos;

        // Running scalar derivative
        dr = pow(r, power - 1.0) * power * dr + 1.0;

        // Use the three coordinate system axis-aligned planes as orbit traps
        // trap = min(trap, abs(w));
    }

    // surf_col = trap;

    // Distance estimate from running derivative and escape radius
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
#ifdef MANDELBULB_SCENE
    // Mandelbulb scene
    return de_mandelbulb(pos);
#else
    // Simple DE test scene
    // float offset = 0.03*sin(20.0*pos.x+in_time)*sin(20.0*pos.y+in_time)*sin(20.0*pos.z+in_time);
    return smin(de_rounded_box(pos, vec3(0.05, 0.85, 0.05), 0.05),
             smin(de_rounded_box(pos, vec3(0.1, 0.1, 0.85), 0.05),
               smin(de_sphere(pos, 0.3),
                 de_torus(pos, 0.8, 0.2),
                   32), 32), 64);
    // return de_cone(pos + vec3(0, -1, 0), normalize(vec2(0.2, 0.1)));
#endif
}

// http://en.wikipedia.org/wiki/Finite_difference#Forward.2C_backward.2C_and_central_differences
vec3 normal_backward_difference(vec3 pos)
{
    float c = distance_estimator(pos);
    const float eps = 0.00001;
    return normalize(vec3(c - distance_estimator(pos - vec3(eps, 0.0, 0.0)),
                          c - distance_estimator(pos - vec3(0.0, eps, 0.0)),
                          c - distance_estimator(pos - vec3(0.0, 0.0, eps))));
}
vec3 normal_central_difference(vec3 pos)
{
    const float eps = 0.00001;
    const vec3 epsX = vec3(eps, 0.0, 0.0);
    const vec3 epsY = vec3(0.0, eps, 0.0);
    const vec3 epsZ = vec3(0.0, 0.0, eps);
    return normalize(vec3(distance_estimator(pos + epsX) - distance_estimator(pos - epsX),
                          distance_estimator(pos + epsY) - distance_estimator(pos - epsY),
                          distance_estimator(pos + epsZ) - distance_estimator(pos - epsZ)));
}

// Compute the world-space surface normal from the screen-space partial derivatives
// of the intersection distance (depth) and the camera transform
vec3 normal_screen_space_depth(float dx, float dy, mat4x4 camera)
{
    // TODO: This is wrong... but doesn't matter, just use normal_screen_space_isec()
    return (camera * vec4(normalize(vec3(dx, dy, sqrt(dx*dx + dy*dy))), 0)).xyz;
}

// Normal from position through screen-space partial derivatives
vec3 normal_screen_space_isec(vec3 p)
{
    return cross(normalize(dFdx(p)), normalize(dFdy(p)));
}

// Distance AO based on the following references:
//
// http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf
// http://www.mazapan.se/news/2010/07/15/gpu-ray-marching-with-distance-fields/
//
//               5    1
// ao = 1 - k *  E   ---  (i * d - distfield(p + n * i * d))
//              i=1  2^i
//
// The above never really seemed to work properly, though. At the very least it
// seems to be required to divide the 'd - distfield' term by d to have it normalized.
//
// Then, there are still errors due to the distance at p not being zero, which makes
// sense as the ray marcher will stop at a min. distance. A cheap fix is to simply clamp
// the term. There's also some kind of surface acne problem that can be mitigated by back
// stepping on the ray like for the normal computation.
//
float distance_ao(vec3 p, vec3 n)
{
    float weight = 0.5;
    float occl_sum = 0.0;
    for (int i=0; i<5; i++)
    {
        // Test progressively larger spheres further away along the surface normal
        float delta = pow(i + 1.0, 4.0) * 0.001; // i = 0..4, delta = 0.001..0.625

        // Check sphere occlusion. The back stepping epsilon seems fairly large, but
        // anything smaller causes issues. The change in position in combination with
        // the min. distance at which the ray marcher stops will cause the occlusion
        // term to leave its range, for now we fix this by simply clamping it instead
        // of trying to account for these errors
        occl_sum += weight * clamp(
            1.0 - distance_estimator((p + n * 0.001) + n * delta) / delta, 0.0, 1.0);

        // More distant, outer spheres contribute exponentially less to the occlusion sum
        weight *= 0.5;
    }

    // Magic fudge factor to make dark parts darker and bright parts brighter
    occl_sum = (clamp((occl_sum * 2 - 1) * 1.65, -1, 1) + 1) * 0.5;
    return pow(1.0 - occl_sum, 8.0);
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

    float h  = sqrt(r2 - a);
    tmin     = t - h;
    tmax     = t + h;

    return true;
}

bool ray_march( vec3 origin
              , vec3 dir
              , out float t             // Intersection T along the ray
              , out float step_gradient // Step count based gradient (for cheap fake AO)
              )
{
    // Ray march till we come close enough to a surface or exceed the iteration count

    const int   MAX_STEPS = 128;
    const float MIN_DIST  = 0.001;

    // First intersect with a bounding sphere. Helps quickly reject rays which can't
    // possibly intersect with the scene and brings our starting point closer
    // to the surface (DEs get very imprecise when we're starting to far away)
#ifdef MANDELBULB_SCENE
    #ifdef POWER8
        float b_sphere_r = 1.25;
    #else
        float b_sphere_r = 1.5;
    #endif
#else
    float b_sphere_r = 1.0;
#endif
    float tspheremin, tspheremax;
    if (!ray_sphere(origin, dir, vec3(0,0,0), b_sphere_r, tspheremin, tspheremax))
        return false;
    t = tspheremin;

    // Ignore intersections behind the origin, might otherwise render scene with flipped
    // ray direction if we're looking away from it
    if (t < 0)
        return false;

    for (int steps=0; steps<MAX_STEPS; steps++)
    {
        vec3 pos = origin + t * dir;
        float dist = distance_estimator(pos);
        t += dist;

        if (t > tspheremax) // Left bounding sphere?
            return false;

        if (dist < MIN_DIST) // Close enough to surface?
        {
            step_gradient = 1.0 - float(steps) / float(MAX_STEPS);
            return true;
        }
    }

    return false;
}

vec3 soft_lam(vec3 n, vec3 light, vec3 surface_col)
{
    vec3  warm_col  = vec3(0.9 , 0.9 , 0.7);
    vec3  cool_col  = vec3(0.07, 0.07, 0.1);
    float diff_warm = 0.35;
    float diff_cool = 0.25;

    float ndotl     = (dot(light, n) + 1.0) * 0.5;

    vec3  kcool     = min((cool_col + diff_cool) * surface_col, 1.0);
    vec3  kwarm     = min((warm_col + diff_warm) * surface_col, 1.0);
    vec3  kfinal    = mix(kcool, kwarm, ndotl);

    return min(kfinal, 1.0);
}

vec3 render_ray(vec3 origin, vec3 dir, mat4x4 camera)
{
    // Ray march
    float t, step_gradient;
    bool hit = ray_march(origin, dir, t, step_gradient);

    // Can use the iteration count to add a snowy/foggy/glow type effect
    //
    // http://www.fractalforums.com/mandelbulb-implementation/faked-ambient-occlusion/
    //     msg10526/#msg10526
    //
    // vec3 glow = (1.0 - pow((clamp((step_gradient * 2 - 1) * 1.5, -1, 1) + 1) * 0.5, 8.0))
    //             * vec3(0.2, 0.3, 0.3);

    if (hit)
    {
        // Compute intersection
        vec3 isec_pos = origin + dir * t;

        // Step back from the surface a bit before computing the normal
        //
        // This epsilon seems to be a good sweet spot for surface acne vs blurriness for
        // the Mandelbulb, replace with something more robust
        //
        // Experiments with trying to step back along the surface normal (cheaply computed
        // in screen-space) did not improve results
        //
        vec3 isec_n = normal_backward_difference(isec_pos - dir * 0.001);

        // TODO: We can fix some numerical problems when computing normals by switching to
        //       screen-space normals when very thin, fin-like surfaces causes errors. This
        //       is most noticeable for some of the lower powers of the mandelbulb, but unfortunately
        //       those surfaces are so disjoint that they also causes issues for our distanced
        //       based AO computations
        //
        // vec3 isec_n_ss = normal_screen_space_isec(isec_pos);
        // if (dot(-dir, isec_n) < 0.0) // Clearly wrong normal?
        //     isec_n = isec_n_ss; // Switch to screen space normal

#define DISTANCE_AO
#ifdef DISTANCE_AO
        float ao = distance_ao(isec_pos, isec_n);
#else
        float ao = pow((clamp((step_gradient * 2 - 1) * 1.25, -1, 1) + 1) * 0.5, 8.0);
#endif

        //if (gl_FragCoord.x < in_screen_wdh / 2)

        // Shading
        //vec3 color = vec3(((isec_n + 1) * 0.5) * ao);
        //vec3 color = soft_lam(isec_n, normalize(vec3(1, 1, 1)), vec3(ao));
        //vec3 color = ((dot(isec_n, (camera * vec4(0, 0, 1, 0)).xyz) +1) * 0.5 + 0.5) * vec3(ao);
        /*vec3 color = clamp(dot(isec_n, vec3(0,0,1)), 0, 1) * vec3(1,0,0) +
                     clamp(dot(isec_n, vec3(0,0,-1)), 0, 1) * vec3(0,1,0);*/
        //vec3 color = (isec_n + 1) * 0.5;
        //vec3 color = vec3(ao);
        /*
        vec3 color = ( vec3(max(0, 0.2+dot(isec_n, normalize(vec3(1, 1, 1))))) * vec3(1,0.75,0.75) +
                       vec3(max(0, 0.2+dot(isec_n, normalize(vec3(-1, -1, -1))))) * vec3(0.75,1.0,1.0)
                     ) * ao;
        */
        vec3 color =
        (
          max(0.2+dot(isec_n, (camera * vec4(0, 0, 1, 0)).xyz),0)*vec3(0.2)+
          vec3(max(0, pow(dot(reflect(isec_n,-dir), normalize(vec3(1,0,1))),5))) * vec3(1,0.4,0)*2 +
          vec3(max(0, pow(dot(reflect(isec_n,-dir), normalize(vec3(1,-1,0))),5))) * vec3(0,.51,.51)*2
        ) * ao;

        return color;
    }
    else
#define BG_GRADIENT
#ifdef BG_GRADIENT
        //return mix(vec3(1, 0.4, 0), vec3(0, 0.51, 0.51), gl_FragCoord.y / in_screen_hgt);
        return texture(env, dir).xyz;
#else
        return vec3(0);
#endif
}

mat4x4 lookat(vec3 eye, vec3 focus, vec3 up)
{
    vec3 zaxis = normalize(eye - focus);
    vec3 xaxis = normalize(cross(up, zaxis));
    vec3 yaxis = cross(zaxis, xaxis);
    return mat4x4(xaxis.x, xaxis.y, xaxis.z, 0.0,
                  yaxis.x, yaxis.y, yaxis.z, 0.0,
                  zaxis.x, zaxis.y, zaxis.z, 0.0,
                  eye.x  , eye.y  , eye.z  , 1.0);
}

void generate_ray( mat4x4 camera       // Camera transform
                 , vec2 sample_offs    // Sample offset [-.5, +.5]
                 , bool ortho          // Orthographic or perspective camera?
                 , float width_or_hfov // Width of ortho viewing volume or horizontal FOV degrees
                 , out vec3 origin
                 , out vec3 dir
                 )
{
    // Convert fragment coordinates and sample offset to NDC [-1, 1]
    vec2 ndc = (gl_FragCoord.xy + sample_offs) / vec2(in_screen_wdh, in_screen_hgt) * 2.0 - 1.0;

    // Generate ray from NDC and camera transform
    float aspect = in_screen_wdh / in_screen_hgt;
    if (ortho)
    {
        // Orthographic projection. Frame [-w/2, w/2] on X, center interval on Y while keeping aspect
        float width  = width_or_hfov;
        float height = width / aspect;
        origin       = (camera * vec4(ndc * vec2(width / 2.0, height / 2.0), 0, 1)).xyz;
        dir          = mat3(camera) * vec3(0, 0, -1);
    }
    else
    {
        // Perspective projection. Unlike the usual vertical FOV we deal with a horizontal
        // one, just like the orthographic camera defined by its width
        float hfov   = radians(width_or_hfov);
        float fov_xs = tan(hfov / 2);
        origin       = (camera * vec4(0, 0, 0, 1)).xyz;
        dir          = mat3(camera) * normalize(vec3(ndc.x*fov_xs, ndc.y*fov_xs / aspect, -1.0));
    }
}

void main()
{
    // Orbit camera
    vec3 cam_pos = vec3(0,0,2);
#define AUTO_ROTATION
#ifdef AUTO_ROTATION
    cam_pos.x = sin(in_time / 3.0);
    cam_pos.z = cos(in_time / 3.0);
    cam_pos.y = cos(in_time / 4.0);
    // Keep a constant distance. Distance is so that a width = 2 orthographic projection
    // matches up with a HFOV = 45 perspective projection as close as possible
    cam_pos = normalize(cam_pos) * 2.414213562373095;
#endif

    // Camera transform. Look at center, orbit around it
    mat4x4 camera = lookat(cam_pos, vec3(0,0,0), vec3(0,1,0));

    // Generate camera ray
    vec3 origin, dir;
//#define CAM_ORTHO
#ifdef CAM_ORTHO
    generate_ray(camera, vec2(0, 0), true, 2.0, origin, dir);
#else
    generate_ray(camera, vec2(0, 0), false, 45.0 * 2, origin, dir);
#endif

    // Trace and shade
    vec3 color = render_ray(origin, dir, camera);

    // Use screen-space derivatives to check the contrast between neighbouring pixels,
    // keep shooting more rays till it passes below a threshold. Works OK from an image
    // quality standpoint, but performance is fairly poor due to the heavy cost of
    // divergence, probably not worth it in practice compared to the naive super sampling
    // we have on the frame buffer level
#ifdef ADAPTIVE_SAMPLING
    float weight = 1.0;
    while (fwidth(pow(color.r / weight, 1.0 / 2.2) /* gamma */) > 0.3 /* threshold*/ && weight < 32)
    {
        // <shoot next ray>
        // weight += 1;
    }
    color /= weight;
#endif

#define GAMMA_CORRECT
#ifdef GAMMA_CORRECT
    // Gamma correct and output
    vec3 gamma = pow(color, vec3(1.0 / 2.2));
    frag_color = vec4(gamma, 1);
#else
    frag_color = vec4(color, 1);
#endif
}

|]

