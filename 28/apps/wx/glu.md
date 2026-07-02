# `glu`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/glu.erl#L31)

Erlang wrapper functions for OpenGL

Standard OpenGL API

This documents the functions as a brief version of the complete
[OpenGL reference pages.](https://www.khronos.org/registry/OpenGL-Refpages/)

# `enum`
*not exported* 

```elixir
-type enum() :: non_neg_integer().
```

# `f`
*not exported* 

```elixir
-type f() :: float().
```

# `i`
*not exported* 

```elixir
-type i() :: integer().
```

# `m12`
*not exported* 

```elixir
-type m12() :: {f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}.
```

# `m16`
*not exported* 

```elixir
-type m16() :: {f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}.
```

# `matrix`
*not exported* 

```elixir
-type matrix() :: m12() | m16().
```

# `mem`
*not exported* 

```elixir
-type mem() :: binary() | tuple().
```

# `vertex`
*not exported* 

```elixir
-type vertex() :: {float(), float(), float()}.
```

# `build1DMipmapLevels`

```elixir
-spec build1DMipmapLevels(Target, InternalFormat, Width, Format, Type, Level, Base, Max, Data) -> i()
                             when
                                 Target :: enum(),
                                 InternalFormat :: i(),
                                 Width :: i(),
                                 Format :: enum(),
                                 Type :: enum(),
                                 Level :: i(),
                                 Base :: i(),
                                 Max :: i(),
                                 Data :: binary().
```

[`glu:build1DMipmapLevels/9`](`build1DMipmapLevels/9`) builds a subset of
prefiltered one-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild1DMipmapLevels.xml)

# `build1DMipmaps`

```elixir
-spec build1DMipmaps(Target, InternalFormat, Width, Format, Type, Data) -> i()
                        when
                            Target :: enum(),
                            InternalFormat :: i(),
                            Width :: i(),
                            Format :: enum(),
                            Type :: enum(),
                            Data :: binary().
```

[`glu:build1DMipmaps/6`](`build1DMipmaps/6`) builds a series of prefiltered
one-dimensional texture maps of decreasing resolutions called a mipmap. This is
used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild1DMipmaps.xml)

# `build2DMipmapLevels`

```elixir
-spec build2DMipmapLevels(Target, InternalFormat, Width, Height, Format, Type, Level, Base, Max, Data) ->
                             i()
                             when
                                 Target :: enum(),
                                 InternalFormat :: i(),
                                 Width :: i(),
                                 Height :: i(),
                                 Format :: enum(),
                                 Type :: enum(),
                                 Level :: i(),
                                 Base :: i(),
                                 Max :: i(),
                                 Data :: binary().
```

[`glu:build2DMipmapLevels/10`](`build2DMipmapLevels/10`) builds a subset of
prefiltered two-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild2DMipmapLevels.xml)

# `build2DMipmaps`

```elixir
-spec build2DMipmaps(Target, InternalFormat, Width, Height, Format, Type, Data) -> i()
                        when
                            Target :: enum(),
                            InternalFormat :: i(),
                            Width :: i(),
                            Height :: i(),
                            Format :: enum(),
                            Type :: enum(),
                            Data :: binary().
```

[`glu:build2DMipmaps/7`](`build2DMipmaps/7`) builds a series of prefiltered
two-dimensional texture maps of decreasing resolutions called a mipmap. This is
used for the antialiasing of texture-mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild2DMipmaps.xml)

# `build3DMipmapLevels`

```elixir
-spec build3DMipmapLevels(Target, InternalFormat, Width, Height, Depth, Format, Type, Level, Base, Max,
                          Data) ->
                             i()
                             when
                                 Target :: enum(),
                                 InternalFormat :: i(),
                                 Width :: i(),
                                 Height :: i(),
                                 Depth :: i(),
                                 Format :: enum(),
                                 Type :: enum(),
                                 Level :: i(),
                                 Base :: i(),
                                 Max :: i(),
                                 Data :: binary().
```

[`glu:build3DMipmapLevels/11`](`build3DMipmapLevels/11`) builds a subset of
prefiltered three-dimensional texture maps of decreasing resolutions called a
mipmap. This is used for the antialiasing of texture mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild3DMipmapLevels.xml)

# `build3DMipmaps`

```elixir
-spec build3DMipmaps(Target, InternalFormat, Width, Height, Depth, Format, Type, Data) -> i()
                        when
                            Target :: enum(),
                            InternalFormat :: i(),
                            Width :: i(),
                            Height :: i(),
                            Depth :: i(),
                            Format :: enum(),
                            Type :: enum(),
                            Data :: binary().
```

[`glu:build3DMipmaps/8`](`build3DMipmaps/8`) builds a series of prefiltered
three-dimensional texture maps of decreasing resolutions called a mipmap. This
is used for the antialiasing of texture-mapped primitives.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluBuild3DMipmaps.xml)

# `checkExtension`

```elixir
-spec checkExtension(ExtName :: string(), ExtString :: string()) -> 0 | 1.
```

[`glu:checkExtension/2`](`checkExtension/2`) returns `?GLU_TRUE` if `ExtName` is
supported otherwise `?GLU_FALSE` is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluCheckExtension.xml)

# `cylinder`

```elixir
-spec cylinder(Quad :: i(), Base :: f(), Top :: f(), Height :: f(), Slices :: i(), Stacks :: i()) -> ok.
```

[`glu:cylinder/6`](`cylinder/6`) draws a cylinder oriented along the `z` axis.
The base of the cylinder is placed at `z` = 0 and the top at z=height. Like a
sphere, a cylinder is subdivided around the `z` axis into slices and along the
`z` axis into stacks.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluCylinder.xml)

# `deleteQuadric`

```elixir
-spec deleteQuadric(Quad :: i()) -> ok.
```

[`glu:deleteQuadric/1`](`deleteQuadric/1`) destroys the quadrics object (created
with [`glu:newQuadric/0`](`newQuadric/0`)) and frees any memory it uses. Once
[`glu:deleteQuadric/1`](`deleteQuadric/1`) has been called, `Quad` cannot be
used again.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluDeleteQuadric.xml)

# `disk`

```elixir
-spec disk(Quad :: i(), Inner :: f(), Outer :: f(), Slices :: i(), Loops :: i()) -> ok.
```

[`glu:disk/5`](`disk/5`) renders a disk on the `z` = 0 plane. The disk has a
radius of `Outer` and contains a concentric circular hole with a radius of
`Inner`. If `Inner` is 0, then no hole is generated. The disk is subdivided
around the `z` axis into slices (like pizza slices) and also about the `z` axis
into rings (as specified by `Slices` and `Loops`, respectively).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluDisk.xml)

# `errorString`

```elixir
-spec errorString(Error :: enum()) -> string().
```

[`glu:errorString/1`](`errorString/1`) produces an error string from a GL or GLU
error code. The string is in ISO Latin 1 format. For example,
[`glu:errorString/1`](`errorString/1`)(`?GLU_OUT_OF_MEMORY`) returns the string
`out of memory`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluErrorString.xml)

# `getString`

```elixir
-spec getString(Name :: enum()) -> string().
```

[`glu:getString/1`](`getString/1`) returns a pointer to a static string
describing the GLU version or the GLU extensions that are supported.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluGetString.xml)

# `lookAt`

```elixir
-spec lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) -> ok
                when
                    EyeX :: f(),
                    EyeY :: f(),
                    EyeZ :: f(),
                    CenterX :: f(),
                    CenterY :: f(),
                    CenterZ :: f(),
                    UpX :: f(),
                    UpY :: f(),
                    UpZ :: f().
```

[`glu:lookAt/9`](`lookAt/9`) creates a viewing matrix derived from an eye point,
a reference point indicating the center of the scene, and an `UP` vector.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluLookAt.xml)

# `newQuadric`

```elixir
-spec newQuadric() -> i().
```

[`glu:newQuadric/0`](`newQuadric/0`) creates and returns a pointer to a new
quadrics object. This object must be referred to when calling quadrics rendering
and control functions. A return value of 0 means that there is not enough memory
to allocate the object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluNewQuadric.xml)

# `ortho2D`

```elixir
-spec ortho2D(Left :: f(), Right :: f(), Bottom :: f(), Top :: f()) -> ok.
```

[`glu:ortho2D/4`](`ortho2D/4`) sets up a two-dimensional orthographic viewing
region. This is equivalent to calling `gl:ortho/6` with near=-1 and far=1.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluOrtho2D.xml)

# `partialDisk`

```elixir
-spec partialDisk(Quad, Inner, Outer, Slices, Loops, Start, Sweep) -> ok
                     when
                         Quad :: i(),
                         Inner :: f(),
                         Outer :: f(),
                         Slices :: i(),
                         Loops :: i(),
                         Start :: f(),
                         Sweep :: f().
```

[`glu:partialDisk/7`](`partialDisk/7`) renders a partial disk on the z=0 plane.
A partial disk is similar to a full disk, except that only the subset of the
disk from `Start` through `Start` \+ `Sweep` is included (where 0 degrees is
along the +f2yf axis, 90 degrees along the +`x` axis, 180 degrees along the -`y`
axis, and 270 degrees along the -`x` axis).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPartialDisk.xml)

# `perspective`

```elixir
-spec perspective(Fovy :: f(), Aspect :: f(), ZNear :: f(), ZFar :: f()) -> ok.
```

[`glu:perspective/4`](`perspective/4`) specifies a viewing frustum into the
world coordinate system. In general, the aspect ratio in
[`glu:perspective/4`](`perspective/4`) should match the aspect ratio of the
associated viewport. For example, aspect=2.0 means the viewer's angle of view is
twice as wide in `x` as it is in `y`. If the viewport is twice as wide as it is
tall, it displays the image without distortion.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml)

# `pickMatrix`

```elixir
-spec pickMatrix(X :: f(), Y :: f(), DelX :: f(), DelY :: f(), Viewport :: {i(), i(), i(), i()}) -> ok.
```

[`glu:pickMatrix/5`](`pickMatrix/5`) creates a projection matrix that can be
used to restrict drawing to a small region of the viewport. This is typically
useful to determine what objects are being drawn near the cursor. Use
[`glu:pickMatrix/5`](`pickMatrix/5`) to restrict drawing to a small region
around the cursor. Then, enter selection mode (with `gl:renderMode/1`) and
rerender the scene. All primitives that would have been drawn near the cursor
are identified and stored in the selection buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPickMatrix.xml)

# `project`

```elixir
-spec project(ObjX, ObjY, ObjZ, Model, Proj, View) -> {i(), WinX :: f(), WinY :: f(), WinZ :: f()}
                 when
                     ObjX :: f(),
                     ObjY :: f(),
                     ObjZ :: f(),
                     Model :: matrix(),
                     Proj :: matrix(),
                     View :: {i(), i(), i(), i()}.
```

[`glu:project/6`](`project/6`) transforms the specified object coordinates into
window coordinates using `Model`, `Proj`, and `View`. The result is stored in
`WinX`, `WinY`, and `WinZ`. A return value of `?GLU_TRUE` indicates success, a
return value of `?GLU_FALSE` indicates failure.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluProject.xml)

# `quadricDrawStyle`

```elixir
-spec quadricDrawStyle(Quad :: i(), Draw :: enum()) -> ok.
```

[`glu:quadricDrawStyle/2`](`quadricDrawStyle/2`) specifies the draw style for
quadrics rendered with `Quad`. The legal values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricDrawStyle.xml)

# `quadricNormals`

```elixir
-spec quadricNormals(Quad :: i(), Normal :: enum()) -> ok.
```

[`glu:quadricNormals/2`](`quadricNormals/2`) specifies what kind of normals are
desired for quadrics rendered with `Quad`. The legal values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricNormals.xml)

# `quadricOrientation`

```elixir
-spec quadricOrientation(Quad :: i(), Orientation :: enum()) -> ok.
```

[`glu:quadricOrientation/2`](`quadricOrientation/2`) specifies what kind of
orientation is desired for quadrics rendered with `Quad`. The `Orientation`
values are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricOrientation.xml)

# `quadricTexture`

```elixir
-spec quadricTexture(Quad :: i(), Texture :: 0 | 1) -> ok.
```

[`glu:quadricTexture/2`](`quadricTexture/2`) specifies if texture coordinates
should be generated for quadrics rendered with `Quad`. If the value of `Texture`
is `?GLU_TRUE`, then texture coordinates are generated, and if `Texture` is
`?GLU_FALSE`, they are not. The initial value is `?GLU_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluQuadricTexture.xml)

# `scaleImage`

```elixir
-spec scaleImage(Format, WIn, HIn, TypeIn, DataIn, WOut, HOut, TypeOut, DataOut) -> i()
                    when
                        Format :: enum(),
                        WIn :: i(),
                        HIn :: i(),
                        TypeIn :: enum(),
                        DataIn :: binary(),
                        WOut :: i(),
                        HOut :: i(),
                        TypeOut :: enum(),
                        DataOut :: mem().
```

[`glu:scaleImage/9`](`scaleImage/9`) scales a pixel image using the appropriate
pixel store modes to unpack data from the source image and pack data into the
destination image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluScaleImage.xml)

# `sphere`

```elixir
-spec sphere(Quad :: i(), Radius :: f(), Slices :: i(), Stacks :: i()) -> ok.
```

[`glu:sphere/4`](`sphere/4`) draws a sphere of the given radius centered around
the origin. The sphere is subdivided around the `z` axis into slices and along
the `z` axis into stacks (similar to lines of longitude and latitude).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluSphere.xml)

# `tesselate`

```elixir
-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}
                   when
                       Normal :: vertex(),
                       Vs :: vertex(),
                       Triangles :: [integer()],
                       VertexPos :: binary().
```

Triangulates a polygon, the polygon is specified by a `Normal` and `Vs` a list
of vertex positions.

The function returns a list of indices of the vertices and a binary (64bit
native float) containing an array of vertex positions, it starts with the
vertices in `Vs` and may contain newly created vertices in the end.

# `unProject4`

```elixir
-spec unProject4(WinX, WinY, WinZ, ClipW, Model, Proj, View, NearVal, FarVal) ->
                    {i(), ObjX :: f(), ObjY :: f(), ObjZ :: f(), ObjW :: f()}
                    when
                        WinX :: f(),
                        WinY :: f(),
                        WinZ :: f(),
                        ClipW :: f(),
                        Model :: matrix(),
                        Proj :: matrix(),
                        View :: {i(), i(), i(), i()},
                        NearVal :: f(),
                        FarVal :: f().
```

[`glu:unProject/6`](`unProject/6`) maps the specified window coordinates into
object coordinates using `Model`, `Proj`, and `View`. The result is stored in
`ObjX`, `ObjY`, and `ObjZ`. A return value of `?GLU_TRUE` indicates success; a
return value of `?GLU_FALSE` indicates failure.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluUnProject.xml)

# `unProject`

```elixir
-spec unProject(WinX, WinY, WinZ, Model, Proj, View) -> {i(), ObjX :: f(), ObjY :: f(), ObjZ :: f()}
                   when
                       WinX :: f(),
                       WinY :: f(),
                       WinZ :: f(),
                       Model :: matrix(),
                       Proj :: matrix(),
                       View :: {i(), i(), i(), i()}.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
