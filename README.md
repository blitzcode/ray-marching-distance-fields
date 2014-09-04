
# Ray Marching Distance Fields

This project is a Haskell and GLSL program containing my distance field / ray marching related experiments. The Haskell viewing application is doing the pre-processing, functioning as a development and debugging aid, allowing different shaders to be explored. It automatically reloads and recompiles shaders when they change, showing an overlay displaying any errors. There's a flexible frame buffer system, supporting under and super sampling and saving of screenshots. Tiled rendering avoids shader timeouts and unresponsive UI. It also features offline convolution of environment maps with a filter kernel to enable fast lighting of diffuse and glossy surfaces. Finally, there's an implementation of the Mandelbulb fractal as well as several simpler ones like Mandelbrot and Julia sets. There's some more functionality in the program, try it out and / or visit my website.

**If you want to read actual algorithm descriptions and references of this project and see more, higher quality images visit the following links to my website**

- [Ray Marching Distance Fields](http://www.blitzcode.net/haskell.shtml#ray_marching_distance_fields)
- [Mandelbulb](http://www.blitzcode.net/haskell.shtml#mandelbulb)
- [Prefiltered Environment Maps](http://www.blitzcode.net/haskell.shtml#prefiltered_environment_maps)
- [Julia Set](http://www.blitzcode.net/haskell.shtml#julia_set)

# Images

A few low-resolution previews of pictures generated, visit the links above for more, higher quality images.

![rmdf](https://raw.github.com/blitzcode/ray-marching-distance-fields/master/img/rmdf.png)
![mandelbulb](https://raw.github.com/blitzcode/ray-marching-distance-fields/master/img/mandelbulb.png)
![prefiltered](https://raw.github.com/blitzcode/ray-marching-distance-fields/master/img/prefiltered.png)
![julia set](https://raw.github.com/blitzcode/ray-marching-distance-fields/master/img/julia_set.png)

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

