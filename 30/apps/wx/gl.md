# `gl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/gl.erl#L31)

Erlang wrapper functions for OpenGL

Standard OpenGL API

This documents the functions as a brief version of the complete
[OpenGL reference pages.](https://www.khronos.org/registry/OpenGL-Refpages/)

# `clamp`
*not exported* 

```erlang
-type clamp() :: float().
```

# `enum`
*not exported* 

```erlang
-type enum() :: non_neg_integer().
```

# `f`
*not exported* 

```erlang
-type f() :: float().
```

# `i`
*not exported* 

```erlang
-type i() :: integer().
```

# `m12`
*not exported* 

```erlang
-type m12() :: {f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}.
```

# `m16`
*not exported* 

```erlang
-type m16() :: {f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}.
```

# `matrix`
*not exported* 

```erlang
-type matrix() :: m12() | m16().
```

# `mem`
*not exported* 

```erlang
-type mem() :: binary() | tuple().
```

# `offset`
*not exported* 

```erlang
-type offset() :: non_neg_integer().
```

# `accum`

```erlang
-spec accum(Op :: enum(), Value :: f()) -> ok.
```

The accumulation buffer is an extended-range color buffer. Images are not
rendered into it. Rather, images rendered into one of the color buffers are
added to the contents of the accumulation buffer after rendering. Effects such
as antialiasing (of points, lines, and polygons), motion blur, and depth of
field can be created by accumulating images generated with different
transformation matrices.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAccum.xml)

# `activeShaderProgram`

```erlang
-spec activeShaderProgram(Pipeline :: i(), Program :: i()) -> ok.
```

[`gl:activeShaderProgram/2`](`activeShaderProgram/2`) sets the linked program
named by `Program` to be the active program for the program pipeline object
`Pipeline`. The active program in the active program pipeline object is the
target of calls to [`gl:uniform()`](`uniform1f/2`) when no program has been made
current through a call to [`gl:useProgram/1`](`useProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveShaderProgram.xhtml)

# `activeTexture`

```erlang
-spec activeTexture(Texture :: enum()) -> ok.
```

[`gl:activeTexture/1`](`activeTexture/1`) selects which texture unit subsequent
texture state calls will affect. The number of texture units an implementation
supports is implementation dependent, but must be at least 80.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveTexture.xhtml)

# `alphaFunc`

```erlang
-spec alphaFunc(Func :: enum(), Ref :: clamp()) -> ok.
```

The alpha test discards fragments depending on the outcome of a comparison
between an incoming fragment's alpha value and a constant reference value.
[`gl:alphaFunc/2`](`alphaFunc/2`) specifies the reference value and the
comparison function. The comparison is performed only if alpha testing is
enabled. By default, it is not enabled. (See [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) of `?GL_ALPHA_TEST`.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAlphaFunc.xml)

# `areTexturesResident`

```erlang
-spec areTexturesResident(Textures :: [i()]) -> {0 | 1, Residences :: [0 | 1]}.
```

GL establishes a \`\`working set'' of textures that are resident in texture
memory. These textures can be bound to a texture target much more efficiently
than textures that are not resident.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glAreTexturesResident.xml)

# `arrayElement`

```erlang
-spec arrayElement(I :: i()) -> ok.
```

[`gl:arrayElement/1`](`arrayElement/1`) commands are used within
[`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pairs to specify
vertex and attribute data for point, line, and polygon primitives. If
`?GL_VERTEX_ARRAY` is enabled when [`gl:arrayElement/1`](`arrayElement/1`) is
called, a single vertex is drawn, using vertex and attribute data taken from
location `I` of the enabled arrays. If `?GL_VERTEX_ARRAY` is not enabled, no
drawing occurs but the attributes corresponding to the enabled arrays are
modified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glArrayElement.xml)

# `attachShader`

```erlang
-spec attachShader(Program :: i(), Shader :: i()) -> ok.
```

In order to create a complete shader program, there must be a way to specify the
list of things that will be linked together. Program objects provide this
mechanism. Shaders that are to be linked together in a program object must first
be attached to that program object. [`gl:attachShader/2`](`attachShader/2`)
attaches the shader object specified by `Shader` to the program object specified
by `Program`. This indicates that `Shader` will be included in link operations
that will be performed on `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glAttachShader.xhtml)

# `begin`

```erlang
-spec 'begin'(Mode :: enum()) -> ok.
```

# `beginConditionalRender`

```erlang
-spec beginConditionalRender(Id :: i(), Mode :: enum()) -> ok.
```

# `beginQuery`

```erlang
-spec beginQuery(Target :: enum(), Id :: i()) -> ok.
```

# `beginQueryIndexed`

```erlang
-spec beginQueryIndexed(Target :: enum(), Index :: i(), Id :: i()) -> ok.
```

# `beginTransformFeedback`

```erlang
-spec beginTransformFeedback(PrimitiveMode :: enum()) -> ok.
```

# `bindAttribLocation`

```erlang
-spec bindAttribLocation(Program :: i(), Index :: i(), Name :: string()) -> ok.
```

[`gl:bindAttribLocation/3`](`bindAttribLocation/3`) is used to associate a
user-defined attribute variable in the program object specified by `Program`
with a generic vertex attribute index. The name of the user-defined attribute
variable is passed as a null terminated string in `Name`. The generic vertex
attribute index to be bound to this variable is specified by `Index`. When
`Program` is made part of current state, values provided via the generic vertex
attribute `Index` will modify the value of the user-defined attribute variable
specified by `Name`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindAttribLocation.xhtml)

# `bindBuffer`

```erlang
-spec bindBuffer(Target :: enum(), Buffer :: i()) -> ok.
```

[`gl:bindBuffer/2`](`bindBuffer/2`) binds a buffer object to the specified
buffer binding point. Calling [`gl:bindBuffer/2`](`bindBuffer/2`) with `Target`
set to one of the accepted symbolic constants and `Buffer` set to the name of a
buffer object binds that buffer object name to the target. If no buffer object
with name `Buffer` exists, one is created with that name. When a buffer object
is bound to a target, the previous binding for that target is automatically
broken.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml)

# `bindBufferBase`

```erlang
-spec bindBufferBase(Target :: enum(), Index :: i(), Buffer :: i()) -> ok.
```

[`gl:bindBufferBase/3`](`bindBufferBase/3`) binds the buffer object `Buffer` to
the binding point at index `Index` of the array of targets specified by
`Target`. Each `Target` represents an indexed array of buffer binding points, as
well as a single general binding point that can be used by other buffer
manipulation functions such as [`gl:bindBuffer/2`](`bindBuffer/2`) or
`glMapBuffer`. In addition to binding `Buffer` to the indexed buffer binding
target, [`gl:bindBufferBase/3`](`bindBufferBase/3`) also binds `Buffer` to the
generic buffer binding point specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferBase.xhtml)

# `bindBufferRange`

```erlang
-spec bindBufferRange(Target :: enum(), Index :: i(), Buffer :: i(), Offset :: i(), Size :: i()) -> ok.
```

[`gl:bindBufferRange/5`](`bindBufferRange/5`) binds a range the buffer object
`Buffer` represented by `Offset` and `Size` to the binding point at index
`Index` of the array of targets specified by `Target`. Each `Target` represents
an indexed array of buffer binding points, as well as a single general binding
point that can be used by other buffer manipulation functions such as
[`gl:bindBuffer/2`](`bindBuffer/2`) or `glMapBuffer`. In addition to binding a
range of `Buffer` to the indexed buffer binding target,
[`gl:bindBufferRange/5`](`bindBufferRange/5`) also binds the range to the
generic buffer binding point specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBufferRange.xhtml)

# `bindBuffersBase`

```erlang
-spec bindBuffersBase(Target :: enum(), First :: i(), Buffers :: [i()]) -> ok.
```

[`gl:bindBuffersBase/3`](`bindBuffersBase/3`) binds a set of `Count` buffer
objects whose names are given in the array `Buffers` to the `Count` consecutive
binding points starting from index `First` of the array of targets specified by
`Target`. If `Buffers` is `?NULL` then
[`gl:bindBuffersBase/3`](`bindBuffersBase/3`) unbinds any buffers that are
currently bound to the referenced binding points. Assuming no errors are
generated, it is equivalent to the following pseudo-code, which calls
[`gl:bindBufferBase/3`](`bindBufferBase/3`), with the exception that the
non-indexed `Target` is not changed by
[`gl:bindBuffersBase/3`](`bindBuffersBase/3`):

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffersBase.xhtml)

# `bindBuffersRange`

```erlang
-spec bindBuffersRange(Target :: enum(),
                       First :: i(),
                       Buffers :: [i()],
                       Offsets :: [i()],
                       Sizes :: [i()]) ->
                          ok.
```

[`gl:bindBuffersRange/5`](`bindBuffersRange/5`) binds a set of `Count` ranges
from buffer objects whose names are given in the array `Buffers` to the `Count`
consecutive binding points starting from index `First` of the array of targets
specified by `Target`. `Offsets` specifies the address of an array containing
`Count` starting offsets within the buffers, and `Sizes` specifies the address
of an array of `Count` sizes of the ranges. If `Buffers` is `?NULL` then
`Offsets` and `Sizes` are ignored and
[`gl:bindBuffersRange/5`](`bindBuffersRange/5`) unbinds any buffers that are
currently bound to the referenced binding points. Assuming no errors are
generated, it is equivalent to the following pseudo-code, which calls
[`gl:bindBufferRange/5`](`bindBufferRange/5`), with the exception that the
non-indexed `Target` is not changed by
[`gl:bindBuffersRange/5`](`bindBuffersRange/5`):

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffersRange.xhtml)

# `bindFragDataLocation`

```erlang
-spec bindFragDataLocation(Program :: i(), Color :: i(), Name :: string()) -> ok.
```

[`gl:bindFragDataLocation/3`](`bindFragDataLocation/3`) explicitly specifies the
binding of the user-defined varying out variable `Name` to fragment shader color
number `ColorNumber` for program `Program`. If `Name` was bound previously, its
assigned binding is replaced with `ColorNumber`. `Name` must be a
null-terminated string. `ColorNumber` must be less than `?GL_MAX_DRAW_BUFFERS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFragDataLocation.xhtml)

# `bindFragDataLocationIndexed`

```erlang
-spec bindFragDataLocationIndexed(Program :: i(), ColorNumber :: i(), Index :: i(), Name :: string()) ->
                                     ok.
```

[`gl:bindFragDataLocationIndexed/4`](`bindFragDataLocationIndexed/4`) specifies
that the varying out variable `Name` in `Program` should be bound to fragment
color `ColorNumber` when the program is next linked. `Index` may be zero or one
to specify that the color be used as either the first or second color input to
the blend equation, respectively.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFragDataLocationIndexed.xhtml)

# `bindFramebuffer`

```erlang
-spec bindFramebuffer(Target :: enum(), Framebuffer :: i()) -> ok.
```

[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) binds the framebuffer object with
name `Framebuffer` to the framebuffer target specified by `Target`. `Target`
must be either `?GL_DRAW_FRAMEBUFFER`, `?GL_READ_FRAMEBUFFER` or
`?GL_FRAMEBUFFER`. If a framebuffer object is bound to `?GL_DRAW_FRAMEBUFFER` or
`?GL_READ_FRAMEBUFFER`, it becomes the target for rendering or readback
operations, respectively, until it is deleted or another framebuffer is bound to
the corresponding bind point. Calling
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) with `Target` set to
`?GL_FRAMEBUFFER` binds `Framebuffer` to both the read and draw framebuffer
targets. `Framebuffer` is the name of a framebuffer object previously returned
from a call to [`gl:genFramebuffers/1`](`genFramebuffers/1`), or zero to break
the existing binding of a framebuffer object to `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFramebuffer.xhtml)

# `bindImageTexture`

```erlang
-spec bindImageTexture(Unit, Texture, Level, Layered, Layer, Access, Format) -> ok
                          when
                              Unit :: i(),
                              Texture :: i(),
                              Level :: i(),
                              Layered :: 0 | 1,
                              Layer :: i(),
                              Access :: enum(),
                              Format :: enum().
```

[`gl:bindImageTexture/7`](`bindImageTexture/7`) binds a single level of a
texture to an image unit for the purpose of reading and writing it from shaders.
`Unit` specifies the zero-based index of the image unit to which to bind the
texture level. `Texture` specifies the name of an existing texture object to
bind to the image unit. If `Texture` is zero, then any existing binding to the
image unit is broken. `Level` specifies the level of the texture to bind to the
image unit.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindImageTexture.xhtml)

# `bindImageTextures`

```erlang
-spec bindImageTextures(First :: i(), Textures :: [i()]) -> ok.
```

[`gl:bindImageTextures/2`](`bindImageTextures/2`) binds images from an array of
existing texture objects to a specified number of consecutive image units.
`Count` specifies the number of texture objects whose names are stored in the
array `Textures`. That number of texture names are read from the array and bound
to the `Count` consecutive texture units starting from `First`. If the name zero
appears in the `Textures` array, any existing binding to the image unit is
reset. Any non-zero entry in `Textures` must be the name of an existing texture
object. When a non-zero entry in `Textures` is present, the image at level zero
is bound, the binding is considered layered, with the first layer set to zero,
and the image is bound for read-write access. The image unit format parameter is
taken from the internal format of the image at level zero of the texture object.
For cube map textures, the internal format of the positive X image of level zero
is used. If `Textures` is `?NULL` then it is as if an appropriately sized array
containing only zeros had been specified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindImageTextures.xhtml)

# `bindProgramPipeline`

```erlang
-spec bindProgramPipeline(Pipeline :: i()) -> ok.
```

[`gl:bindProgramPipeline/1`](`bindProgramPipeline/1`) binds a program pipeline
object to the current context. `Pipeline` must be a name previously returned
from a call to [`gl:genProgramPipelines/1`](`genProgramPipelines/1`). If no
program pipeline exists with name `Pipeline` then a new pipeline object is
created with that name and initialized to the default state vector.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindProgramPipeline.xhtml)

# `bindRenderbuffer`

```erlang
-spec bindRenderbuffer(Target :: enum(), Renderbuffer :: i()) -> ok.
```

[`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) binds the renderbuffer object
with name `Renderbuffer` to the renderbuffer target specified by `Target`.
`Target` must be `?GL_RENDERBUFFER`. `Renderbuffer` is the name of a
renderbuffer object previously returned from a call to
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`), or zero to break the existing
binding of a renderbuffer object to `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindRenderbuffer.xhtml)

# `bindSampler`

```erlang
-spec bindSampler(Unit :: i(), Sampler :: i()) -> ok.
```

[`gl:bindSampler/2`](`bindSampler/2`) binds `Sampler` to the texture unit at
index `Unit`. `Sampler` must be zero or the name of a sampler object previously
returned from a call to [`gl:genSamplers/1`](`genSamplers/1`). `Unit` must be
less than the value of `?GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindSampler.xhtml)

# `bindSamplers`

```erlang
-spec bindSamplers(First :: i(), Samplers :: [i()]) -> ok.
```

[`gl:bindSamplers/2`](`bindSamplers/2`) binds samplers from an array of existing
sampler objects to a specified number of consecutive sampler units. `Count`
specifies the number of sampler objects whose names are stored in the array
`Samplers`. That number of sampler names is read from the array and bound to the
`Count` consecutive sampler units starting from `First`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindSamplers.xhtml)

# `bindTexture`

```erlang
-spec bindTexture(Target :: enum(), Texture :: i()) -> ok.
```

[`gl:bindTexture/2`](`bindTexture/2`) lets you create or use a named texture.
Calling [`gl:bindTexture/2`](`bindTexture/2`) with `Target` set to
`?GL_TEXTURE_1D`, `?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, `?GL_TEXTURE_BUFFER`, `?GL_TEXTURE_2D_MULTISAMPLE`
or `?GL_TEXTURE_2D_MULTISAMPLE_ARRAY` and `Texture` set to the name of the new
texture binds the texture name to the target. When a texture is bound to a
target, the previous binding for that target is automatically broken.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTexture.xhtml)

# `bindTextures`

```erlang
-spec bindTextures(First :: i(), Textures :: [i()]) -> ok.
```

[`gl:bindTextures/2`](`bindTextures/2`) binds an array of existing texture
objects to a specified number of consecutive texture units. `Count` specifies
the number of texture objects whose names are stored in the array `Textures`.
That number of texture names are read from the array and bound to the `Count`
consecutive texture units starting from `First`. The target, or type of texture
is deduced from the texture object and each texture is bound to the
corresponding target of the texture unit. If the name zero appears in the
`Textures` array, any existing binding to any target of the texture unit is
reset and the default texture for that target is bound in its place. Any
non-zero entry in `Textures` must be the name of an existing texture object. If
`Textures` is `?NULL` then it is as if an appropriately sized array containing
only zeros had been specified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTextures.xhtml)

# `bindTextureUnit`

```erlang
-spec bindTextureUnit(Unit :: i(), Texture :: i()) -> ok.
```

[`gl:bindTextureUnit/2`](`bindTextureUnit/2`) binds an existing texture object
to the texture unit numbered `Unit`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTextureUnit.xhtml)

# `bindTransformFeedback`

```erlang
-spec bindTransformFeedback(Target :: enum(), Id :: i()) -> ok.
```

[`gl:bindTransformFeedback/2`](`bindTransformFeedback/2`) binds the transform
feedback object with name `Id` to the current GL state. `Id` must be a name
previously returned from a call to
[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`). If `Id` has not
previously been bound, a new transform feedback object with name `Id` and
initialized with the default transform state vector is created.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTransformFeedback.xhtml)

# `bindVertexArray`

```erlang
-spec bindVertexArray(Array :: i()) -> ok.
```

[`gl:bindVertexArray/1`](`bindVertexArray/1`) binds the vertex array object with
name `Array`. `Array` is the name of a vertex array object previously returned
from a call to [`gl:genVertexArrays/1`](`genVertexArrays/1`), or zero to break
the existing vertex array object binding.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexArray.xhtml)

# `bindVertexBuffer`

```erlang
-spec bindVertexBuffer(Bindingindex :: i(), Buffer :: i(), Offset :: i(), Stride :: i()) -> ok.
```

# `bindVertexBuffers`

```erlang
-spec bindVertexBuffers(First :: i(), Buffers :: [i()], Offsets :: [i()], Strides :: [i()]) -> ok.
```

# `bitmap`

```erlang
-spec bitmap(Width, Height, Xorig, Yorig, Xmove, Ymove, Bitmap) -> ok
                when
                    Width :: i(),
                    Height :: i(),
                    Xorig :: f(),
                    Yorig :: f(),
                    Xmove :: f(),
                    Ymove :: f(),
                    Bitmap :: offset() | mem().
```

A bitmap is a binary image. When drawn, the bitmap is positioned relative to the
current raster position, and frame buffer pixels corresponding to 1's in the
bitmap are written using the current raster color or index. Frame buffer pixels
corresponding to 0's in the bitmap are not modified.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBitmap.xml)

# `blendColor`

```erlang
-spec blendColor(Red :: clamp(), Green :: clamp(), Blue :: clamp(), Alpha :: clamp()) -> ok.
```

The `?GL_BLEND_COLOR` may be used to calculate the source and destination
blending factors. The color components are clamped to the range \[0 1] before
being stored. See [`gl:blendFunc/2`](`blendFunc/2`) for a complete description
of the blending operations. Initially the `?GL_BLEND_COLOR` is set to (0, 0, 0,
0).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendColor.xhtml)

# `blendEquation`

```erlang
-spec blendEquation(Mode :: enum()) -> ok.
```

# `blendEquationi`

```erlang
-spec blendEquationi(Buf :: i(), Mode :: enum()) -> ok.
```

The blend equations determine how a new pixel (the ''source'' color) is combined
with a pixel already in the framebuffer (the ''destination'' color). This
function sets both the RGB blend equation and the alpha blend equation to a
single equation. [`gl:blendEquationi/2`](`blendEquation/1`) specifies the blend
equation for a single draw buffer whereas
[`gl:blendEquation/1`](`blendEquation/1`) sets the blend equation for all draw
buffers.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquation.xhtml)

# `blendEquationSeparate`

```erlang
-spec blendEquationSeparate(ModeRGB :: enum(), ModeAlpha :: enum()) -> ok.
```

# `blendEquationSeparatei`

```erlang
-spec blendEquationSeparatei(Buf :: i(), ModeRGB :: enum(), ModeAlpha :: enum()) -> ok.
```

The blend equations determines how a new pixel (the ''source'' color) is
combined with a pixel already in the framebuffer (the ''destination'' color).
These functions specify one blend equation for the RGB-color components and one
blend equation for the alpha component.
[`gl:blendEquationSeparatei/3`](`blendEquationSeparate/2`) specifies the blend
equations for a single draw buffer whereas
[`gl:blendEquationSeparate/2`](`blendEquationSeparate/2`) sets the blend
equations for all draw buffers.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendEquationSeparate.xhtml)

# `blendFunc`

```erlang
-spec blendFunc(Sfactor :: enum(), Dfactor :: enum()) -> ok.
```

# `blendFunci`

```erlang
-spec blendFunci(Buf :: i(), Src :: enum(), Dst :: enum()) -> ok.
```

Pixels can be drawn using a function that blends the incoming (source) RGBA
values with the RGBA values that are already in the frame buffer (the
destination values). Blending is initially disabled. Use
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) with argument
`?GL_BLEND` to enable and disable blending.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFunc.xhtml)

# `blendFuncSeparate`

```erlang
-spec blendFuncSeparate(SfactorRGB, DfactorRGB, SfactorAlpha, DfactorAlpha) -> ok
                           when
                               SfactorRGB :: enum(),
                               DfactorRGB :: enum(),
                               SfactorAlpha :: enum(),
                               DfactorAlpha :: enum().
```

# `blendFuncSeparatei`

```erlang
-spec blendFuncSeparatei(Buf :: i(),
                         SrcRGB :: enum(),
                         DstRGB :: enum(),
                         SrcAlpha :: enum(),
                         DstAlpha :: enum()) ->
                            ok.
```

Pixels can be drawn using a function that blends the incoming (source) RGBA
values with the RGBA values that are already in the frame buffer (the
destination values). Blending is initially disabled. Use
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) with argument
`?GL_BLEND` to enable and disable blending.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlendFuncSeparate.xhtml)

# `blitFramebuffer`

```erlang
-spec blitFramebuffer(SrcX0, SrcY0, SrcX1, SrcY1, DstX0, DstY0, DstX1, DstY1, Mask, Filter) -> ok
                         when
                             SrcX0 :: i(),
                             SrcY0 :: i(),
                             SrcX1 :: i(),
                             SrcY1 :: i(),
                             DstX0 :: i(),
                             DstY0 :: i(),
                             DstX1 :: i(),
                             DstY1 :: i(),
                             Mask :: i(),
                             Filter :: enum().
```

[`gl:blitFramebuffer/10`](`blitFramebuffer/10`) and `glBlitNamedFramebuffer`
transfer a rectangle of pixel values from one region of a read framebuffer to
another region of a draw framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBlitFramebuffer.xhtml)

# `bufferData`

```erlang
-spec bufferData(Target :: enum(), Size :: i(), Data :: offset() | mem(), Usage :: enum()) -> ok.
```

[`gl:bufferData/4`](`bufferData/4`) and `glNamedBufferData` create a new data
store for a buffer object. In case of [`gl:bufferData/4`](`bufferData/4`), the
buffer object currently bound to `Target` is used. For `glNamedBufferData`, a
buffer object associated with ID specified by the caller in `Buffer` will be
used instead.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml)

# `bufferStorage`

```erlang
-spec bufferStorage(Target :: enum(), Size :: i(), Data :: offset() | mem(), Flags :: i()) -> ok.
```

[`gl:bufferStorage/4`](`bufferStorage/4`) and `glNamedBufferStorage` create a
new immutable data store. For [`gl:bufferStorage/4`](`bufferStorage/4`), the
buffer object currently bound to `Target` will be initialized. For
`glNamedBufferStorage`, `Buffer` is the name of the buffer object that will be
configured. The size of the data store is specified by `Size`. If an initial
data is available, its address may be supplied in `Data`. Otherwise, to create
an uninitialized data store, `Data` should be `?NULL`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferStorage.xhtml)

# `bufferSubData`

```erlang
-spec bufferSubData(Target :: enum(), Offset :: i(), Size :: i(), Data :: offset() | mem()) -> ok.
```

[`gl:bufferSubData/4`](`bufferSubData/4`) and `glNamedBufferSubData` redefine
some or all of the data store for the specified buffer object. Data starting at
byte offset `Offset` and extending for `Size` bytes is copied to the data store
from the memory pointed to by `Data`. `Offset` and `Size` must define a range
lying entirely within the buffer object's data store.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml)

# `callList`

```erlang
-spec callList(List :: i()) -> ok.
```

[`gl:callList/1`](`callList/1`) causes the named display list to be executed.
The commands saved in the display list are executed in order, just as if they
were called without using a display list. If `List` has not been defined as a
display list, [`gl:callList/1`](`callList/1`) is ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallList.xml)

# `callLists`

```erlang
-spec callLists(Lists :: [i()]) -> ok.
```

[`gl:callLists/1`](`callLists/1`) causes each display list in the list of names
passed as `Lists` to be executed. As a result, the commands saved in each
display list are executed in order, just as if they were called without using a
display list. Names of display lists that have not been defined are ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCallLists.xml)

# `checkFramebufferStatus`

```erlang
-spec checkFramebufferStatus(Target :: enum()) -> enum().
```

[`gl:checkFramebufferStatus/1`](`checkFramebufferStatus/1`) and
`glCheckNamedFramebufferStatus` return the completeness status of a framebuffer
object when treated as a read or draw framebuffer, depending on the value of
`Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCheckFramebufferStatus.xhtml)

# `clampColor`

```erlang
-spec clampColor(Target :: enum(), Clamp :: enum()) -> ok.
```

[`gl:clampColor/2`](`clampColor/2`) controls color clamping that is performed
during [`gl:readPixels/7`](`readPixels/7`). `Target` must be
`?GL_CLAMP_READ_COLOR`. If `Clamp` is `?GL_TRUE`, read color clamping is
enabled; if `Clamp` is `?GL_FALSE`, read color clamping is disabled. If `Clamp`
is `?GL_FIXED_ONLY`, read color clamping is enabled only if the selected read
buffer has fixed point components and disabled otherwise.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClampColor.xhtml)

# `clear`

```erlang
-spec clear(Mask :: i()) -> ok.
```

[`gl:clear/1`](`clear/1`) sets the bitplane area of the window to values
previously selected by [`gl:clearColor/4`](`clearColor/4`),
[`gl:clearDepth/1`](`clearDepth/1`), and
[`gl:clearStencil/1`](`clearStencil/1`). Multiple color buffers can be cleared
simultaneously by selecting more than one buffer at a time using
[`gl:drawBuffer/1`](`drawBuffer/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClear.xhtml)

# `clearAccum`

```erlang
-spec clearAccum(Red :: f(), Green :: f(), Blue :: f(), Alpha :: f()) -> ok.
```

[`gl:clearAccum/4`](`clearAccum/4`) specifies the red, green, blue, and alpha
values used by [`gl:clear/1`](`clear/1`) to clear the accumulation buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearAccum.xml)

# `clearBufferData`

```erlang
-spec clearBufferData(Target, Internalformat, Format, Type, Data) -> ok
                         when
                             Target :: enum(),
                             Internalformat :: enum(),
                             Format :: enum(),
                             Type :: enum(),
                             Data :: offset() | mem().
```

# `clearBufferfi`

```erlang
-spec clearBufferfi(Buffer :: enum(), Drawbuffer :: i(), Depth :: f(), Stencil :: i()) -> ok.
```

# `clearBufferfv`

```erlang
-spec clearBufferfv(Buffer :: enum(), Drawbuffer :: i(), Value :: tuple()) -> ok.
```

# `clearBufferiv`

```erlang
-spec clearBufferiv(Buffer :: enum(), Drawbuffer :: i(), Value :: tuple()) -> ok.
```

# `clearBufferSubData`

```erlang
-spec clearBufferSubData(Target, Internalformat, Offset, Size, Format, Type, Data) -> ok
                            when
                                Target :: enum(),
                                Internalformat :: enum(),
                                Offset :: i(),
                                Size :: i(),
                                Format :: enum(),
                                Type :: enum(),
                                Data :: offset() | mem().
```

# `clearBufferuiv`

```erlang
-spec clearBufferuiv(Buffer :: enum(), Drawbuffer :: i(), Value :: tuple()) -> ok.
```

These commands clear a specified buffer of a framebuffer to specified value(s).
For [`gl:clearBuffer*()`](`clearBufferiv/3`), the framebuffer is the currently
bound draw framebuffer object. For `glClearNamedFramebuffer*`, `Framebuffer` is
zero, indicating the default draw framebuffer, or the name of a framebuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearBuffer.xhtml)

# `clearColor`

```erlang
-spec clearColor(Red :: clamp(), Green :: clamp(), Blue :: clamp(), Alpha :: clamp()) -> ok.
```

[`gl:clearColor/4`](`clearColor/4`) specifies the red, green, blue, and alpha
values used by [`gl:clear/1`](`clear/1`) to clear the color buffers. Values
specified by [`gl:clearColor/4`](`clearColor/4`) are clamped to the range \[0
1].

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearColor.xhtml)

# `clearDepth`

```erlang
-spec clearDepth(Depth :: clamp()) -> ok.
```

# `clearDepthf`

```erlang
-spec clearDepthf(D :: f()) -> ok.
```

[`gl:clearDepth/1`](`clearDepth/1`) specifies the depth value used by
[`gl:clear/1`](`clear/1`) to clear the depth buffer. Values specified by
[`gl:clearDepth/1`](`clearDepth/1`) are clamped to the range \[0 1].

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearDepth.xhtml)

# `clearIndex`

```erlang
-spec clearIndex(C :: f()) -> ok.
```

[`gl:clearIndex/1`](`clearIndex/1`) specifies the index used by
[`gl:clear/1`](`clear/1`) to clear the color index buffers. `C` is not clamped.
Rather, `C` is converted to a fixed-point value with unspecified precision to
the right of the binary point. The integer part of this value is then masked
with 2 m-1, where m is the number of bits in a color index stored in the frame
buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearIndex.xml)

# `clearStencil`

```erlang
-spec clearStencil(S :: i()) -> ok.
```

[`gl:clearStencil/1`](`clearStencil/1`) specifies the index used by
[`gl:clear/1`](`clear/1`) to clear the stencil buffer. `S` is masked with 2 m-1,
where m is the number of bits in the stencil buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearStencil.xhtml)

# `clearTexImage`

```erlang
-spec clearTexImage(Texture :: i(),
                    Level :: i(),
                    Format :: enum(),
                    Type :: enum(),
                    Data :: offset() | mem()) ->
                       ok.
```

[`gl:clearTexImage/5`](`clearTexImage/5`) fills all an image contained in a
texture with an application supplied value. `Texture` must be the name of an
existing texture. Further, `Texture` may not be the name of a buffer texture,
nor may its internal format be compressed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearTexImage.xhtml)

# `clearTexSubImage`

```erlang
-spec clearTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type,
                       Data) ->
                          ok
                          when
                              Texture :: i(),
                              Level :: i(),
                              Xoffset :: i(),
                              Yoffset :: i(),
                              Zoffset :: i(),
                              Width :: i(),
                              Height :: i(),
                              Depth :: i(),
                              Format :: enum(),
                              Type :: enum(),
                              Data :: offset() | mem().
```

[`gl:clearTexSubImage/11`](`clearTexSubImage/11`) fills all or part of an image
contained in a texture with an application supplied value. `Texture` must be the
name of an existing texture. Further, `Texture` may not be the name of a buffer
texture, nor may its internal format be compressed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClearTexSubImage.xhtml)

# `clientActiveTexture`

```erlang
-spec clientActiveTexture(Texture :: enum()) -> ok.
```

[`gl:clientActiveTexture/1`](`clientActiveTexture/1`) selects the vertex array
client state parameters to be modified by
[`gl:texCoordPointer/4`](`texCoordPointer/4`), and enabled or disabled with
[`gl:enableClientState/1`](`enableClientState/1`) or
[`gl:disableClientState/1`](`enableClientState/1`), respectively, when called
with a parameter of `?GL_TEXTURE_COORD_ARRAY`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClientActiveTexture.xml)

# `clientWaitSync`

```erlang
-spec clientWaitSync(Sync :: i(), Flags :: i(), Timeout :: i()) -> enum().
```

[`gl:clientWaitSync/3`](`clientWaitSync/3`) causes the client to block and wait
for the sync object specified by `Sync` to become signaled. If `Sync` is
signaled when [`gl:clientWaitSync/3`](`clientWaitSync/3`) is called,
[`gl:clientWaitSync/3`](`clientWaitSync/3`) returns immediately, otherwise it
will block and wait for up to `Timeout` nanoseconds for `Sync` to become
signaled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClientWaitSync.xhtml)

# `clipControl`

```erlang
-spec clipControl(Origin :: enum(), Depth :: enum()) -> ok.
```

[`gl:clipControl/2`](`clipControl/2`) controls the clipping volume behavior and
the clip coordinate to window coordinate transformation behavior.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClipControl.xhtml)

# `clipPlane`

```erlang
-spec clipPlane(Plane :: enum(), Equation :: {f(), f(), f(), f()}) -> ok.
```

Geometry is always clipped against the boundaries of a six-plane frustum in `x`,
`y`, and `z`. [`gl:clipPlane/2`](`clipPlane/2`) allows the specification of
additional planes, not necessarily perpendicular to the `x`, `y`, or `z` axis,
against which all geometry is clipped. To determine the maximum number of
additional clipping planes, call [`gl:getIntegerv/1`](`getBooleanv/1`) with
argument `?GL_MAX_CLIP_PLANES`. All implementations support at least six such
clipping planes. Because the resulting clipping region is the intersection of
the defined half-spaces, it is always convex.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClipPlane.xml)

# `color3b`

```erlang
-spec color3b(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3bv`

```erlang
-spec color3bv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color3d`

```erlang
-spec color3d(Red :: f(), Green :: f(), Blue :: f()) -> ok.
```

# `color3dv`

```erlang
-spec color3dv({Red :: f(), Green :: f(), Blue :: f()}) -> ok.
```

# `color3f`

```erlang
-spec color3f(Red :: f(), Green :: f(), Blue :: f()) -> ok.
```

# `color3fv`

```erlang
-spec color3fv({Red :: f(), Green :: f(), Blue :: f()}) -> ok.
```

# `color3i`

```erlang
-spec color3i(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3iv`

```erlang
-spec color3iv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color3s`

```erlang
-spec color3s(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3sv`

```erlang
-spec color3sv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color3ub`

```erlang
-spec color3ub(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3ubv`

```erlang
-spec color3ubv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color3ui`

```erlang
-spec color3ui(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3uiv`

```erlang
-spec color3uiv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color3us`

```erlang
-spec color3us(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `color3usv`

```erlang
-spec color3usv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `color4b`

```erlang
-spec color4b(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4bv`

```erlang
-spec color4bv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

# `color4d`

```erlang
-spec color4d(Red :: f(), Green :: f(), Blue :: f(), Alpha :: f()) -> ok.
```

# `color4dv`

```erlang
-spec color4dv({Red :: f(), Green :: f(), Blue :: f(), Alpha :: f()}) -> ok.
```

# `color4f`

```erlang
-spec color4f(Red :: f(), Green :: f(), Blue :: f(), Alpha :: f()) -> ok.
```

# `color4fv`

```erlang
-spec color4fv({Red :: f(), Green :: f(), Blue :: f(), Alpha :: f()}) -> ok.
```

# `color4i`

```erlang
-spec color4i(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4iv`

```erlang
-spec color4iv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

# `color4s`

```erlang
-spec color4s(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4sv`

```erlang
-spec color4sv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

# `color4ub`

```erlang
-spec color4ub(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4ubv`

```erlang
-spec color4ubv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

# `color4ui`

```erlang
-spec color4ui(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4uiv`

```erlang
-spec color4uiv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

# `color4us`

```erlang
-spec color4us(Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()) -> ok.
```

# `color4usv`

```erlang
-spec color4usv({Red :: i(), Green :: i(), Blue :: i(), Alpha :: i()}) -> ok.
```

The GL stores both a current single-valued color index and a current four-valued
RGBA color. [`gl:color()`](`color3b/3`) sets a new four-valued RGBA color.
[`gl:color()`](`color3b/3`) has two major variants: [`gl:color3()`](`color3b/3`)
and [`gl:color4()`](`color3b/3`). [`gl:color3()`](`color3b/3`) variants specify
new red, green, and blue values explicitly and set the current alpha value to
1.0 (full intensity) implicitly. [`gl:color4()`](`color3b/3`) variants specify
all four color components explicitly.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColor.xml)

# `colorMask`

```erlang
-spec colorMask(Red :: 0 | 1, Green :: 0 | 1, Blue :: 0 | 1, Alpha :: 0 | 1) -> ok.
```

# `colorMaski`

```erlang
-spec colorMaski(Index :: i(), R :: 0 | 1, G :: 0 | 1, B :: 0 | 1, A :: 0 | 1) -> ok.
```

[`gl:colorMask/4`](`colorMask/4`) and [`gl:colorMaski/5`](`colorMask/4`) specify
whether the individual color components in the frame buffer can or cannot be
written. [`gl:colorMaski/5`](`colorMask/4`) sets the mask for a specific draw
buffer, whereas [`gl:colorMask/4`](`colorMask/4`) sets the mask for all draw
buffers. If `Red` is `?GL_FALSE`, for example, no change is made to the red
component of any pixel in any of the color buffers, regardless of the drawing
operation attempted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glColorMask.xhtml)

# `colorMaterial`

```erlang
-spec colorMaterial(Face :: enum(), Mode :: enum()) -> ok.
```

[`gl:colorMaterial/2`](`colorMaterial/2`) specifies which material parameters
track the current color. When `?GL_COLOR_MATERIAL` is enabled, the material
parameter or parameters specified by `Mode`, of the material or materials
specified by `Face`, track the current color at all times.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorMaterial.xml)

# `colorPointer`

```erlang
-spec colorPointer(Size :: i(), Type :: enum(), Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:colorPointer/4`](`colorPointer/4`) specifies the location and data format
of an array of color components to use when rendering. `Size` specifies the
number of components per color, and must be 3 or 4. `Type` specifies the data
type of each color component, and `Stride` specifies the byte stride from one
color to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays. (Single-array storage may be more efficient
on some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorPointer.xml)

# `colorSubTable`

```erlang
-spec colorSubTable(Target, Start, Count, Format, Type, Data) -> ok
                       when
                           Target :: enum(),
                           Start :: i(),
                           Count :: i(),
                           Format :: enum(),
                           Type :: enum(),
                           Data :: offset() | mem().
```

[`gl:colorSubTable/6`](`colorSubTable/6`) is used to respecify a contiguous
portion of a color table previously defined using
[`gl:colorTable/6`](`colorTable/6`). The pixels referenced by `Data` replace the
portion of the existing table from indices `Start` to start+count-1, inclusive.
This region may not include any entries outside the range of the color table as
it was originally specified. It is not an error to specify a subtexture with
width of 0, but such a specification has no effect.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorSubTable.xml)

# `colorTable`

```erlang
-spec colorTable(Target, Internalformat, Width, Format, Type, Table) -> ok
                    when
                        Target :: enum(),
                        Internalformat :: enum(),
                        Width :: i(),
                        Format :: enum(),
                        Type :: enum(),
                        Table :: offset() | mem().
```

[`gl:colorTable/6`](`colorTable/6`) may be used in two ways: to test the actual
size and color resolution of a lookup table given a particular set of
parameters, or to load the contents of a color lookup table. Use the targets
`?GL_PROXY_*` for the first case and the other targets for the second case.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTable.xml)

# `colorTableParameterfv`

```erlang
-spec colorTableParameterfv(Target :: enum(), Pname :: enum(), Params :: {f(), f(), f(), f()}) -> ok.
```

# `colorTableParameteriv`

```erlang
-spec colorTableParameteriv(Target :: enum(), Pname :: enum(), Params :: {i(), i(), i(), i()}) -> ok.
```

[`gl:colorTableParameter()`](`colorTableParameterfv/3`) is used to specify the
scale factors and bias terms applied to color components when they are loaded
into a color table. `Target` indicates which color table the scale and bias
terms apply to; it must be set to `?GL_COLOR_TABLE`,
`?GL_POST_CONVOLUTION_COLOR_TABLE`, or `?GL_POST_COLOR_MATRIX_COLOR_TABLE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glColorTableParameter.xml)

# `compileShader`

```erlang
-spec compileShader(Shader :: i()) -> ok.
```

[`gl:compileShader/1`](`compileShader/1`) compiles the source code strings that
have been stored in the shader object specified by `Shader`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompileShader.xhtml)

# `compressedTexImage1D`

```erlang
-spec compressedTexImage1D(Target, Level, Internalformat, Width, Border, ImageSize, Data) -> ok
                              when
                                  Target :: enum(),
                                  Level :: i(),
                                  Internalformat :: enum(),
                                  Width :: i(),
                                  Border :: i(),
                                  ImageSize :: i(),
                                  Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage1D.xhtml)

# `compressedTexImage2D`

```erlang
-spec compressedTexImage2D(Target, Level, Internalformat, Width, Height, Border, ImageSize, Data) -> ok
                              when
                                  Target :: enum(),
                                  Level :: i(),
                                  Internalformat :: enum(),
                                  Width :: i(),
                                  Height :: i(),
                                  Border :: i(),
                                  ImageSize :: i(),
                                  Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage2D.xhtml)

# `compressedTexImage3D`

```erlang
-spec compressedTexImage3D(Target, Level, Internalformat, Width, Height, Depth, Border, ImageSize, Data) ->
                              ok
                              when
                                  Target :: enum(),
                                  Level :: i(),
                                  Internalformat :: enum(),
                                  Width :: i(),
                                  Height :: i(),
                                  Depth :: i(),
                                  Border :: i(),
                                  ImageSize :: i(),
                                  Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexImage3D.xhtml)

# `compressedTexSubImage1D`

```erlang
-spec compressedTexSubImage1D(Target, Level, Xoffset, Width, Format, ImageSize, Data) -> ok
                                 when
                                     Target :: enum(),
                                     Level :: i(),
                                     Xoffset :: i(),
                                     Width :: i(),
                                     Format :: enum(),
                                     ImageSize :: i(),
                                     Data :: offset() | mem().
```

# `compressedTexSubImage2D`

```erlang
-spec compressedTexSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize, Data) ->
                                 ok
                                 when
                                     Target :: enum(),
                                     Level :: i(),
                                     Xoffset :: i(),
                                     Yoffset :: i(),
                                     Width :: i(),
                                     Height :: i(),
                                     Format :: enum(),
                                     ImageSize :: i(),
                                     Data :: offset() | mem().
```

# `compressedTexSubImage3D`

```erlang
-spec compressedTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format,
                              ImageSize, Data) ->
                                 ok
                                 when
                                     Target :: enum(),
                                     Level :: i(),
                                     Xoffset :: i(),
                                     Yoffset :: i(),
                                     Zoffset :: i(),
                                     Width :: i(),
                                     Height :: i(),
                                     Depth :: i(),
                                     Format :: enum(),
                                     ImageSize :: i(),
                                     Data :: offset() | mem().
```

# `compressedTextureSubImage1D`

```erlang
-spec compressedTextureSubImage1D(Texture, Level, Xoffset, Width, Format, ImageSize, Data) -> ok
                                     when
                                         Texture :: i(),
                                         Level :: i(),
                                         Xoffset :: i(),
                                         Width :: i(),
                                         Format :: enum(),
                                         ImageSize :: i(),
                                         Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage1D.xhtml)

# `compressedTextureSubImage2D`

```erlang
-spec compressedTextureSubImage2D(Texture, Level, Xoffset, Yoffset, Width, Height, Format, ImageSize,
                                  Data) ->
                                     ok
                                     when
                                         Texture :: i(),
                                         Level :: i(),
                                         Xoffset :: i(),
                                         Yoffset :: i(),
                                         Width :: i(),
                                         Height :: i(),
                                         Format :: enum(),
                                         ImageSize :: i(),
                                         Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage2D.xhtml)

# `compressedTextureSubImage3D`

```erlang
-spec compressedTextureSubImage3D(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth,
                                  Format, ImageSize, Data) ->
                                     ok
                                     when
                                         Texture :: i(),
                                         Level :: i(),
                                         Xoffset :: i(),
                                         Yoffset :: i(),
                                         Zoffset :: i(),
                                         Width :: i(),
                                         Height :: i(),
                                         Depth :: i(),
                                         Format :: enum(),
                                         ImageSize :: i(),
                                         Data :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCompressedTexSubImage3D.xhtml)

# `convolutionFilter1D`

```erlang
-spec convolutionFilter1D(Target, Internalformat, Width, Format, Type, Image) -> ok
                             when
                                 Target :: enum(),
                                 Internalformat :: enum(),
                                 Width :: i(),
                                 Format :: enum(),
                                 Type :: enum(),
                                 Image :: offset() | mem().
```

[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`) builds a one-dimensional
convolution filter kernel from an array of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter1D.xml)

# `convolutionFilter2D`

```erlang
-spec convolutionFilter2D(Target, Internalformat, Width, Height, Format, Type, Image) -> ok
                             when
                                 Target :: enum(),
                                 Internalformat :: enum(),
                                 Width :: i(),
                                 Height :: i(),
                                 Format :: enum(),
                                 Type :: enum(),
                                 Image :: offset() | mem().
```

[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`) builds a two-dimensional
convolution filter kernel from an array of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionFilter2D.xml)

# `convolutionParameterf`

```erlang
-spec convolutionParameterf(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `convolutionParameterfv`

```erlang
-spec convolutionParameterfv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `convolutionParameteri`

```erlang
-spec convolutionParameteri(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `convolutionParameteriv`

```erlang
-spec convolutionParameteriv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:convolutionParameter()`](`convolutionParameterf/3`) sets the value of a
convolution parameter.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glConvolutionParameter.xml)

# `copyBufferSubData`

```erlang
-spec copyBufferSubData(ReadTarget, WriteTarget, ReadOffset, WriteOffset, Size) -> ok
                           when
                               ReadTarget :: enum(),
                               WriteTarget :: enum(),
                               ReadOffset :: i(),
                               WriteOffset :: i(),
                               Size :: i().
```

[`gl:copyBufferSubData/5`](`copyBufferSubData/5`) and `glCopyNamedBufferSubData`
copy part of the data store attached to a source buffer object to the data store
attached to a destination buffer object. The number of basic machine units
indicated by `Size` is copied from the source at offset `ReadOffset` to the
destination at `WriteOffset`. `ReadOffset`, `WriteOffset` and `Size` are in
terms of basic machine units.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyBufferSubData.xhtml)

# `copyColorSubTable`

```erlang
-spec copyColorSubTable(Target :: enum(), Start :: i(), X :: i(), Y :: i(), Width :: i()) -> ok.
```

[`gl:copyColorSubTable/5`](`copyColorSubTable/5`) is used to respecify a
contiguous portion of a color table previously defined using
[`gl:colorTable/6`](`colorTable/6`). The pixels copied from the framebuffer
replace the portion of the existing table from indices `Start` to start+x-1,
inclusive. This region may not include any entries outside the range of the
color table, as was originally specified. It is not an error to specify a
subtexture with width of 0, but such a specification has no effect.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorSubTable.xml)

# `copyColorTable`

```erlang
-spec copyColorTable(Target :: enum(), Internalformat :: enum(), X :: i(), Y :: i(), Width :: i()) -> ok.
```

[`gl:copyColorTable/5`](`copyColorTable/5`) loads a color table with pixels from
the current `?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:colorTable/6`](`colorTable/6`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyColorTable.xml)

# `copyConvolutionFilter1D`

```erlang
-spec copyConvolutionFilter1D(Target :: enum(),
                              Internalformat :: enum(),
                              X :: i(),
                              Y :: i(),
                              Width :: i()) ->
                                 ok.
```

[`gl:copyConvolutionFilter1D/5`](`copyConvolutionFilter1D/5`) defines a
one-dimensional convolution filter kernel with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter1D.xml)

# `copyConvolutionFilter2D`

```erlang
-spec copyConvolutionFilter2D(Target :: enum(),
                              Internalformat :: enum(),
                              X :: i(),
                              Y :: i(),
                              Width :: i(),
                              Height :: i()) ->
                                 ok.
```

[`gl:copyConvolutionFilter2D/6`](`copyConvolutionFilter2D/6`) defines a
two-dimensional convolution filter kernel with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyConvolutionFilter2D.xml)

# `copyImageSubData`

```erlang
-spec copyImageSubData(SrcName, SrcTarget, SrcLevel, SrcX, SrcY, SrcZ, DstName, DstTarget, DstLevel,
                       DstX, DstY, DstZ, SrcWidth, SrcHeight, SrcDepth) ->
                          ok
                          when
                              SrcName :: i(),
                              SrcTarget :: enum(),
                              SrcLevel :: i(),
                              SrcX :: i(),
                              SrcY :: i(),
                              SrcZ :: i(),
                              DstName :: i(),
                              DstTarget :: enum(),
                              DstLevel :: i(),
                              DstX :: i(),
                              DstY :: i(),
                              DstZ :: i(),
                              SrcWidth :: i(),
                              SrcHeight :: i(),
                              SrcDepth :: i().
```

[`gl:copyImageSubData/15`](`copyImageSubData/15`) may be used to copy data from
one image (i.e. texture or renderbuffer) to another.
[`gl:copyImageSubData/15`](`copyImageSubData/15`) does not perform
general-purpose conversions such as scaling, resizing, blending, color-space, or
format conversions. It should be considered to operate in a manner similar to a
CPU memcpy. CopyImageSubData can copy between images with different internal
formats, provided the formats are compatible.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyImageSubData.xhtml)

# `copyPixels`

```erlang
-spec copyPixels(X :: i(), Y :: i(), Width :: i(), Height :: i(), Type :: enum()) -> ok.
```

[`gl:copyPixels/5`](`copyPixels/5`) copies a screen-aligned rectangle of pixels
from the specified frame buffer location to a region relative to the current
raster position. Its operation is well defined only if the entire pixel source
region is within the exposed portion of the window. Results of copies from
outside the window, or from regions of the window that are not exposed, are
hardware dependent and undefined.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glCopyPixels.xml)

# `copyTexImage1D`

```erlang
-spec copyTexImage1D(Target, Level, Internalformat, X, Y, Width, Border) -> ok
                        when
                            Target :: enum(),
                            Level :: i(),
                            Internalformat :: enum(),
                            X :: i(),
                            Y :: i(),
                            Width :: i(),
                            Border :: i().
```

[`gl:copyTexImage1D/7`](`copyTexImage1D/7`) defines a one-dimensional texture
image with pixels from the current `?GL_READ_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage1D.xhtml)

# `copyTexImage2D`

```erlang
-spec copyTexImage2D(Target, Level, Internalformat, X, Y, Width, Height, Border) -> ok
                        when
                            Target :: enum(),
                            Level :: i(),
                            Internalformat :: enum(),
                            X :: i(),
                            Y :: i(),
                            Width :: i(),
                            Height :: i(),
                            Border :: i().
```

[`gl:copyTexImage2D/8`](`copyTexImage2D/8`) defines a two-dimensional texture
image, or cube-map texture image with pixels from the current `?GL_READ_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexImage2D.xhtml)

# `copyTexSubImage1D`

```erlang
-spec copyTexSubImage1D(Target :: enum(),
                        Level :: i(),
                        Xoffset :: i(),
                        X :: i(),
                        Y :: i(),
                        Width :: i()) ->
                           ok.
```

[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`) and `glCopyTextureSubImage1D`
replace a portion of a one-dimensional texture image with pixels from the
current `?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:texSubImage1D/7`](`texSubImage1D/7`)). For
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`), the texture object that is
bound to `Target` will be used for the process. For `glCopyTextureSubImage1D`,
`Texture` tells which texture object should be used for the purpose of the call.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage1D.xhtml)

# `copyTexSubImage2D`

```erlang
-spec copyTexSubImage2D(Target, Level, Xoffset, Yoffset, X, Y, Width, Height) -> ok
                           when
                               Target :: enum(),
                               Level :: i(),
                               Xoffset :: i(),
                               Yoffset :: i(),
                               X :: i(),
                               Y :: i(),
                               Width :: i(),
                               Height :: i().
```

[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`) and `glCopyTextureSubImage2D`
replace a rectangular portion of a two-dimensional texture image, cube-map
texture image, rectangular image, or a linear portion of a number of slices of a
one-dimensional array texture with pixels from the current `?GL_READ_BUFFER`
(rather than from main memory, as is the case for
[`gl:texSubImage2D/9`](`texSubImage2D/9`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage2D.xhtml)

# `copyTexSubImage3D`

```erlang
-spec copyTexSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, X, Y, Width, Height) -> ok
                           when
                               Target :: enum(),
                               Level :: i(),
                               Xoffset :: i(),
                               Yoffset :: i(),
                               Zoffset :: i(),
                               X :: i(),
                               Y :: i(),
                               Width :: i(),
                               Height :: i().
```

[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`) and `glCopyTextureSubImage3D`
functions replace a rectangular portion of a three-dimensional or
two-dimensional array texture image with pixels from the current
`?GL_READ_BUFFER` (rather than from main memory, as is the case for
[`gl:texSubImage3D/11`](`texSubImage3D/11`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCopyTexSubImage3D.xhtml)

# `createBuffers`

```erlang
-spec createBuffers(N :: i()) -> [i()].
```

[`gl:createBuffers/1`](`createBuffers/1`) returns `N` previously unused buffer
names in `Buffers`, each representing a new buffer object initialized as if it
had been bound to an unspecified target.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateBuffers.xhtml)

# `createFramebuffers`

```erlang
-spec createFramebuffers(N :: i()) -> [i()].
```

[`gl:createFramebuffers/1`](`createFramebuffers/1`) returns `N` previously
unused framebuffer names in `Framebuffers`, each representing a new framebuffer
object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateFramebuffers.xhtml)

# `createProgram`

```erlang
-spec createProgram() -> i().
```

[`gl:createProgram/0`](`createProgram/0`) creates an empty program object and
returns a non-zero value by which it can be referenced. A program object is an
object to which shader objects can be attached. This provides a mechanism to
specify the shader objects that will be linked to create a program. It also
provides a means for checking the compatibility of the shaders that will be used
to create a program (for instance, checking the compatibility between a vertex
shader and a fragment shader). When no longer needed as part of a program
object, shader objects can be detached.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateProgram.xhtml)

# `createProgramPipelines`

```erlang
-spec createProgramPipelines(N :: i()) -> [i()].
```

[`gl:createProgramPipelines/1`](`createProgramPipelines/1`) returns `N`
previously unused program pipeline names in `Pipelines`, each representing a new
program pipeline object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateProgramPipelines.xhtml)

# `createQueries`

```erlang
-spec createQueries(Target :: enum(), N :: i()) -> [i()].
```

[`gl:createQueries/2`](`createQueries/2`) returns `N` previously unused query
object names in `Ids`, each representing a new query object with the specified
`Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateQueries.xhtml)

# `createRenderbuffers`

```erlang
-spec createRenderbuffers(N :: i()) -> [i()].
```

[`gl:createRenderbuffers/1`](`createRenderbuffers/1`) returns `N` previously
unused renderbuffer object names in `Renderbuffers`, each representing a new
renderbuffer object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateRenderbuffers.xhtml)

# `createSamplers`

```erlang
-spec createSamplers(N :: i()) -> [i()].
```

[`gl:createSamplers/1`](`createSamplers/1`) returns `N` previously unused
sampler names in `Samplers`, each representing a new sampler object initialized
to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateSamplers.xhtml)

# `createShader`

```erlang
-spec createShader(Type :: enum()) -> i().
```

[`gl:createShader/1`](`createShader/1`) creates an empty shader object and
returns a non-zero value by which it can be referenced. A shader object is used
to maintain the source code strings that define a shader. `ShaderType` indicates
the type of shader to be created. Five types of shader are supported. A shader
of type `?GL_COMPUTE_SHADER` is a shader that is intended to run on the
programmable compute processor. A shader of type `?GL_VERTEX_SHADER` is a shader
that is intended to run on the programmable vertex processor. A shader of type
`?GL_TESS_CONTROL_SHADER` is a shader that is intended to run on the
programmable tessellation processor in the control stage. A shader of type
`?GL_TESS_EVALUATION_SHADER` is a shader that is intended to run on the
programmable tessellation processor in the evaluation stage. A shader of type
`?GL_GEOMETRY_SHADER` is a shader that is intended to run on the programmable
geometry processor. A shader of type `?GL_FRAGMENT_SHADER` is a shader that is
intended to run on the programmable fragment processor.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateShader.xhtml)

# `createShaderProgramv`

```erlang
-spec createShaderProgramv(Type :: enum(), Strings :: [unicode:chardata()]) -> i().
```

[`gl:createShaderProgram()`](`createShaderProgramv/2`) creates a program object
containing compiled and linked shaders for a single stage specified by `Type`.
`Strings` refers to an array of `Count` strings from which to create the shader
executables.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateShaderProgram.xhtml)

# `createTextures`

```erlang
-spec createTextures(Target :: enum(), N :: i()) -> [i()].
```

[`gl:createTextures/2`](`createTextures/2`) returns `N` previously unused
texture names in `Textures`, each representing a new texture object of the
dimensionality and type specified by `Target` and initialized to the default
values for that texture type.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateTextures.xhtml)

# `createTransformFeedbacks`

```erlang
-spec createTransformFeedbacks(N :: i()) -> [i()].
```

[`gl:createTransformFeedbacks/1`](`createTransformFeedbacks/1`) returns `N`
previously unused transform feedback object names in `Ids`, each representing a
new transform feedback object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateTransformFeedbacks.xhtml)

# `createVertexArrays`

```erlang
-spec createVertexArrays(N :: i()) -> [i()].
```

[`gl:createVertexArrays/1`](`createVertexArrays/1`) returns `N` previously
unused vertex array object names in `Arrays`, each representing a new vertex
array object initialized to the default state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCreateVertexArrays.xhtml)

# `cullFace`

```erlang
-spec cullFace(Mode :: enum()) -> ok.
```

[`gl:cullFace/1`](`cullFace/1`) specifies whether front- or back-facing facets
are culled (as specified by `mode`) when facet culling is enabled. Facet culling
is initially disabled. To enable and disable facet culling, call the
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) commands with the
argument `?GL_CULL_FACE`. Facets include triangles, quadrilaterals, polygons,
and rectangles.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCullFace.xhtml)

# `debugMessageControl`

```erlang
-spec debugMessageControl(Source :: enum(),
                          Type :: enum(),
                          Severity :: enum(),
                          Ids :: [i()],
                          Enabled :: 0 | 1) ->
                             ok.
```

[`gl:debugMessageControl/5`](`debugMessageControl/5`) controls the reporting of
debug messages generated by a debug context. The parameters `Source`, `Type` and
`Severity` form a filter to select messages from the pool of potential messages
generated by the GL.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDebugMessageControl.xhtml)

# `debugMessageInsert`

```erlang
-spec debugMessageInsert(Source :: enum(),
                         Type :: enum(),
                         Id :: i(),
                         Severity :: enum(),
                         Buf :: string()) ->
                            ok.
```

[`gl:debugMessageInsert/5`](`debugMessageInsert/5`) inserts a user-supplied
message into the debug output queue. `Source` specifies the source that will be
used to classify the message and must be `?GL_DEBUG_SOURCE_APPLICATION` or
`?GL_DEBUG_SOURCE_THIRD_PARTY`. All other sources are reserved for use by the GL
implementation. `Type` indicates the type of the message to be inserted and may
be one of `?GL_DEBUG_TYPE_ERROR`, `?GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR`,
`?GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR`, `?GL_DEBUG_TYPE_PORTABILITY`,
`?GL_DEBUG_TYPE_PERFORMANCE`, `?GL_DEBUG_TYPE_MARKER`,
`?GL_DEBUG_TYPE_PUSH_GROUP`, `?GL_DEBUG_TYPE_POP_GROUP`, or
`?GL_DEBUG_TYPE_OTHER`. `Severity` indicates the severity of the message and may
be `?GL_DEBUG_SEVERITY_LOW`, `?GL_DEBUG_SEVERITY_MEDIUM`,
`?GL_DEBUG_SEVERITY_HIGH` or `?GL_DEBUG_SEVERITY_NOTIFICATION`. `Id` is
available for application defined use and may be any value. This value will be
recorded and used to identify the message.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDebugMessageInsert.xhtml)

# `deleteBuffers`

```erlang
-spec deleteBuffers(Buffers :: [i()]) -> ok.
```

[`gl:deleteBuffers/1`](`deleteBuffers/1`) deletes `N` buffer objects named by
the elements of the array `Buffers`. After a buffer object is deleted, it has no
contents, and its name is free for reuse (for example by
[`gl:genBuffers/1`](`genBuffers/1`)). If a buffer object that is currently bound
is deleted, the binding reverts to 0 (the absence of any buffer object).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteBuffers.xhtml)

# `deleteFramebuffers`

```erlang
-spec deleteFramebuffers(Framebuffers :: [i()]) -> ok.
```

[`gl:deleteFramebuffers/1`](`deleteFramebuffers/1`) deletes the `N` framebuffer
objects whose names are stored in the array addressed by `Framebuffers`. The
name zero is reserved by the GL and is silently ignored, should it occur in
`Framebuffers`, as are other unused names. Once a framebuffer object is deleted,
its name is again unused and it has no attachments. If a framebuffer that is
currently bound to one or more of the targets `?GL_DRAW_FRAMEBUFFER` or
`?GL_READ_FRAMEBUFFER` is deleted, it is as though
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`) had been executed with the
corresponding `Target` and `Framebuffer` zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteFramebuffers.xhtml)

# `deleteLists`

```erlang
-spec deleteLists(List :: i(), Range :: i()) -> ok.
```

[`gl:deleteLists/2`](`deleteLists/2`) causes a contiguous group of display lists
to be deleted. `List` is the name of the first display list to be deleted, and
`Range` is the number of display lists to delete. All display lists d with
list&lt;= d&lt;= list+range-1 are deleted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDeleteLists.xml)

# `deleteProgram`

```erlang
-spec deleteProgram(Program :: i()) -> ok.
```

[`gl:deleteProgram/1`](`deleteProgram/1`) frees the memory and invalidates the
name associated with the program object specified by `Program.` This command
effectively undoes the effects of a call to
[`gl:createProgram/0`](`createProgram/0`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgram.xhtml)

# `deleteProgramPipelines`

```erlang
-spec deleteProgramPipelines(Pipelines :: [i()]) -> ok.
```

[`gl:deleteProgramPipelines/1`](`deleteProgramPipelines/1`) deletes the `N`
program pipeline objects whose names are stored in the array `Pipelines`. Unused
names in `Pipelines` are ignored, as is the name zero. After a program pipeline
object is deleted, its name is again unused and it has no contents. If program
pipeline object that is currently bound is deleted, the binding for that object
reverts to zero and no program pipeline object becomes current.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteProgramPipelines.xhtml)

# `deleteQueries`

```erlang
-spec deleteQueries(Ids :: [i()]) -> ok.
```

[`gl:deleteQueries/1`](`deleteQueries/1`) deletes `N` query objects named by the
elements of the array `Ids`. After a query object is deleted, it has no
contents, and its name is free for reuse (for example by
[`gl:genQueries/1`](`genQueries/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteQueries.xhtml)

# `deleteRenderbuffers`

```erlang
-spec deleteRenderbuffers(Renderbuffers :: [i()]) -> ok.
```

[`gl:deleteRenderbuffers/1`](`deleteRenderbuffers/1`) deletes the `N`
renderbuffer objects whose names are stored in the array addressed by
`Renderbuffers`. The name zero is reserved by the GL and is silently ignored,
should it occur in `Renderbuffers`, as are other unused names. Once a
renderbuffer object is deleted, its name is again unused and it has no contents.
If a renderbuffer that is currently bound to the target `?GL_RENDERBUFFER` is
deleted, it is as though [`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) had
been executed with a `Target` of `?GL_RENDERBUFFER` and a `Name` of zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteRenderbuffers.xhtml)

# `deleteSamplers`

```erlang
-spec deleteSamplers(Samplers :: [i()]) -> ok.
```

[`gl:deleteSamplers/1`](`deleteSamplers/1`) deletes `N` sampler objects named by
the elements of the array `Samplers`. After a sampler object is deleted, its
name is again unused. If a sampler object that is currently bound to a sampler
unit is deleted, it is as though [`gl:bindSampler/2`](`bindSampler/2`) is called
with unit set to the unit the sampler is bound to and sampler zero. Unused names
in samplers are silently ignored, as is the reserved name zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSamplers.xhtml)

# `deleteShader`

```erlang
-spec deleteShader(Shader :: i()) -> ok.
```

[`gl:deleteShader/1`](`deleteShader/1`) frees the memory and invalidates the
name associated with the shader object specified by `Shader`. This command
effectively undoes the effects of a call to
[`gl:createShader/1`](`createShader/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteShader.xhtml)

# `deleteSync`

```erlang
-spec deleteSync(Sync :: i()) -> ok.
```

[`gl:deleteSync/1`](`deleteSync/1`) deletes the sync object specified by `Sync`.
If the fence command corresponding to the specified sync object has completed,
or if no [`gl:waitSync/3`](`waitSync/3`) or
[`gl:clientWaitSync/3`](`clientWaitSync/3`) commands are blocking on `Sync`, the
object is deleted immediately. Otherwise, `Sync` is flagged for deletion and
will be deleted when it is no longer associated with any fence command and is no
longer blocking any [`gl:waitSync/3`](`waitSync/3`) or
[`gl:clientWaitSync/3`](`clientWaitSync/3`) command. In either case, after
[`gl:deleteSync/1`](`deleteSync/1`) returns, the name `Sync` is invalid and can
no longer be used to refer to the sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteSync.xhtml)

# `deleteTextures`

```erlang
-spec deleteTextures(Textures :: [i()]) -> ok.
```

[`gl:deleteTextures/1`](`deleteTextures/1`) deletes `N` textures named by the
elements of the array `Textures`. After a texture is deleted, it has no contents
or dimensionality, and its name is free for reuse (for example by
[`gl:genTextures/1`](`genTextures/1`)). If a texture that is currently bound is
deleted, the binding reverts to 0 (the default texture).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTextures.xhtml)

# `deleteTransformFeedbacks`

```erlang
-spec deleteTransformFeedbacks(Ids :: [i()]) -> ok.
```

[`gl:deleteTransformFeedbacks/1`](`deleteTransformFeedbacks/1`) deletes the `N`
transform feedback objects whose names are stored in the array `Ids`. Unused
names in `Ids` are ignored, as is the name zero. After a transform feedback
object is deleted, its name is again unused and it has no contents. If an active
transform feedback object is deleted, its name immediately becomes unused, but
the underlying object is not deleted until it is no longer active.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteTransformFeedbacks.xhtml)

# `deleteVertexArrays`

```erlang
-spec deleteVertexArrays(Arrays :: [i()]) -> ok.
```

[`gl:deleteVertexArrays/1`](`deleteVertexArrays/1`) deletes `N` vertex array
objects whose names are stored in the array addressed by `Arrays`. Once a vertex
array object is deleted it has no contents and its name is again unused. If a
vertex array object that is currently bound is deleted, the binding for that
object reverts to zero and the default vertex array becomes current. Unused
names in `Arrays` are silently ignored, as is the value zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDeleteVertexArrays.xhtml)

# `depthFunc`

```erlang
-spec depthFunc(Func :: enum()) -> ok.
```

[`gl:depthFunc/1`](`depthFunc/1`) specifies the function used to compare each
incoming pixel depth value with the depth value present in the depth buffer. The
comparison is performed only if depth testing is enabled. (See
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) of
`?GL_DEPTH_TEST`.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthFunc.xhtml)

# `depthMask`

```erlang
-spec depthMask(Flag :: 0 | 1) -> ok.
```

[`gl:depthMask/1`](`depthMask/1`) specifies whether the depth buffer is enabled
for writing. If `Flag` is `?GL_FALSE`, depth buffer writing is disabled.
Otherwise, it is enabled. Initially, depth buffer writing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthMask.xhtml)

# `depthRange`

```erlang
-spec depthRange(Near_val :: clamp(), Far_val :: clamp()) -> ok.
```

# `depthRangeArrayv`

```erlang
-spec depthRangeArrayv(First :: i(), V :: [{f(), f()}]) -> ok.
```

After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes. Each viewport has an
independent depth range specified as a linear mapping of the normalized depth
coordinates in this range to window depth coordinates. Regardless of the actual
depth buffer implementation, window coordinate depth values are treated as
though they range from 0 through 1 (like color components).
[`gl:depthRangeArray()`](`depthRangeArrayv/2`) specifies a linear mapping of the
normalized depth coordinates in this range to window depth coordinates for each
viewport in the range [`First`, `First` \+ `Count`). Thus, the values accepted
by [`gl:depthRangeArray()`](`depthRangeArrayv/2`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRangeArray.xhtml)

# `depthRangef`

```erlang
-spec depthRangef(N :: f(), F :: f()) -> ok.
```

After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes.
[`gl:depthRange/2`](`depthRange/2`) specifies a linear mapping of the normalized
depth coordinates in this range to window depth coordinates. Regardless of the
actual depth buffer implementation, window coordinate depth values are treated
as though they range from 0 through 1 (like color components). Thus, the values
accepted by [`gl:depthRange/2`](`depthRange/2`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRange.xhtml)

# `depthRangeIndexed`

```erlang
-spec depthRangeIndexed(Index :: i(), N :: f(), F :: f()) -> ok.
```

After clipping and division by `w`, depth coordinates range from -1 to 1,
corresponding to the near and far clipping planes. Each viewport has an
independent depth range specified as a linear mapping of the normalized depth
coordinates in this range to window depth coordinates. Regardless of the actual
depth buffer implementation, window coordinate depth values are treated as
though they range from 0 through 1 (like color components).
[`gl:depthRangeIndexed/3`](`depthRangeIndexed/3`) specifies a linear mapping of
the normalized depth coordinates in this range to window depth coordinates for a
specified viewport. Thus, the values accepted by
[`gl:depthRangeIndexed/3`](`depthRangeIndexed/3`) are both clamped to this range
before they are accepted.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDepthRangeIndexed.xhtml)

# `detachShader`

```erlang
-spec detachShader(Program :: i(), Shader :: i()) -> ok.
```

[`gl:detachShader/2`](`detachShader/2`) detaches the shader object specified by
`Shader` from the program object specified by `Program`. This command can be
used to undo the effect of the command [`gl:attachShader/2`](`attachShader/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDetachShader.xhtml)

# `disable`

```erlang
-spec disable(Cap :: enum()) -> ok.
```

# `disableClientState`

```erlang
-spec disableClientState(Cap :: enum()) -> ok.
```

# `disablei`

```erlang
-spec disablei(Target :: enum(), Index :: i()) -> ok.
```

# `disableVertexArrayAttrib`

```erlang
-spec disableVertexArrayAttrib(Vaobj :: i(), Index :: i()) -> ok.
```

# `disableVertexAttribArray`

```erlang
-spec disableVertexAttribArray(Index :: i()) -> ok.
```

# `dispatchCompute`

```erlang
-spec dispatchCompute(Num_groups_x :: i(), Num_groups_y :: i(), Num_groups_z :: i()) -> ok.
```

[`gl:dispatchCompute/3`](`dispatchCompute/3`) launches one or more compute work
groups. Each work group is processed by the active program object for the
compute shader stage. While the individual shader invocations within a work
group are executed as a unit, work groups are executed completely independently
and in unspecified order. `Num_groups_x`, `Num_groups_y` and `Num_groups_z`
specify the number of local work groups that will be dispatched in the X, Y and
Z dimensions, respectively.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDispatchCompute.xhtml)

# `dispatchComputeIndirect`

```erlang
-spec dispatchComputeIndirect(Indirect :: i()) -> ok.
```

[`gl:dispatchComputeIndirect/1`](`dispatchComputeIndirect/1`) launches one or
more compute work groups using parameters stored in the buffer object currently
bound to the `?GL_DISPATCH_INDIRECT_BUFFER` target. Each work group is processed
by the active program object for the compute shader stage. While the individual
shader invocations within a work group are executed as a unit, work groups are
executed completely independently and in unspecified order. `Indirect` contains
the offset into the data store of the buffer object bound to the
`?GL_DISPATCH_INDIRECT_BUFFER` target at which the parameters are stored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDispatchComputeIndirect.xhtml)

# `drawArrays`

```erlang
-spec drawArrays(Mode :: enum(), First :: i(), Count :: i()) -> ok.
```

[`gl:drawArrays/3`](`drawArrays/3`) specifies multiple geometric primitives with
very few subroutine calls. Instead of calling a GL procedure to pass each
individual vertex, normal, texture coordinate, edge flag, or color, you can
prespecify separate arrays of vertices, normals, and colors and use them to
construct a sequence of primitives with a single call to
[`gl:drawArrays/3`](`drawArrays/3`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml)

# `drawArraysIndirect`

```erlang
-spec drawArraysIndirect(Mode :: enum(), Indirect :: offset() | mem()) -> ok.
```

[`gl:drawArraysIndirect/2`](`drawArraysIndirect/2`) specifies multiple geometric
primitives with very few subroutine calls.
[`gl:drawArraysIndirect/2`](`drawArraysIndirect/2`) behaves similarly to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`),
execept that the parameters to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
are stored in memory at the address given by `Indirect`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysIndirect.xhtml)

# `drawArraysInstanced`

```erlang
-spec drawArraysInstanced(Mode :: enum(), First :: i(), Count :: i(), Instancecount :: i()) -> ok.
```

[`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`) behaves identically to
[`gl:drawArrays/3`](`drawArrays/3`) except that `Instancecount` instances of the
range of elements are executed and the value of the internal counter
`InstanceID` advances for each iteration. `InstanceID` is an internal 32-bit
integer counter that may be read by a vertex shader as `?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysInstanced.xhtml)

# `drawArraysInstancedBaseInstance`

```erlang
-spec drawArraysInstancedBaseInstance(Mode :: enum(),
                                      First :: i(),
                                      Count :: i(),
                                      Instancecount :: i(),
                                      Baseinstance :: i()) ->
                                         ok.
```

[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
behaves identically to [`gl:drawArrays/3`](`drawArrays/3`) except that
`Instancecount` instances of the range of elements are executed and the value of
the internal counter `InstanceID` advances for each iteration. `InstanceID` is
an internal 32-bit integer counter that may be read by a vertex shader as
`?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArraysInstancedBaseInstance.xhtml)

# `drawBuffer`

```erlang
-spec drawBuffer(Mode :: enum()) -> ok.
```

When colors are written to the frame buffer, they are written into the color
buffers specified by [`gl:drawBuffer/1`](`drawBuffer/1`). One of the following
values can be used for default framebuffer:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffer.xhtml)

# `drawBuffers`

```erlang
-spec drawBuffers(Bufs :: [enum()]) -> ok.
```

[`gl:drawBuffers/1`](`drawBuffers/1`) and `glNamedFramebufferDrawBuffers` define
an array of buffers into which outputs from the fragment shader data will be
written. If a fragment shader writes a value to one or more user defined output
variables, then the value of each variable will be written into the buffer
specified at a location within `Bufs` corresponding to the location assigned to
that user defined output. The draw buffer used for user defined outputs assigned
to locations greater than or equal to `N` is implicitly set to `?GL_NONE` and
any data written to such an output is discarded.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffers.xhtml)

# `drawElements`

```erlang
-spec drawElements(Mode :: enum(), Count :: i(), Type :: enum(), Indices :: offset() | mem()) -> ok.
```

[`gl:drawElements/4`](`drawElements/4`) specifies multiple geometric primitives
with very few subroutine calls. Instead of calling a GL function to pass each
individual vertex, normal, texture coordinate, edge flag, or color, you can
prespecify separate arrays of vertices, normals, and so on, and use them to
construct a sequence of primitives with a single call to
[`gl:drawElements/4`](`drawElements/4`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElements.xhtml)

# `drawElementsBaseVertex`

```erlang
-spec drawElementsBaseVertex(Mode, Count, Type, Indices, Basevertex) -> ok
                                when
                                    Mode :: enum(),
                                    Count :: i(),
                                    Type :: enum(),
                                    Indices :: offset() | mem(),
                                    Basevertex :: i().
```

[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`) behaves identically
to [`gl:drawElements/4`](`drawElements/4`) except that the `i`th element
transferred by the corresponding draw call will be taken from element
`Indices`\[i] + `Basevertex` of each enabled array. If the resulting value is
larger than the maximum value representable by `Type`, it is as if the
calculation were upconverted to 32-bit unsigned integers (with wrapping on
overflow conditions). The operation is undefined if the sum would be negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsBaseVertex.xhtml)

# `drawElementsIndirect`

```erlang
-spec drawElementsIndirect(Mode :: enum(), Type :: enum(), Indirect :: offset() | mem()) -> ok.
```

[`gl:drawElementsIndirect/3`](`drawElementsIndirect/3`) specifies multiple
indexed geometric primitives with very few subroutine calls.
[`gl:drawElementsIndirect/3`](`drawElementsIndirect/3`) behaves similarly to
[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`),
execpt that the parameters to
[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`)
are stored in memory at the address given by `Indirect`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsIndirect.xhtml)

# `drawElementsInstanced`

```erlang
-spec drawElementsInstanced(Mode, Count, Type, Indices, Instancecount) -> ok
                               when
                                   Mode :: enum(),
                                   Count :: i(),
                                   Type :: enum(),
                                   Indices :: offset() | mem(),
                                   Instancecount :: i().
```

[`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`) behaves identically to
[`gl:drawElements/4`](`drawElements/4`) except that `Instancecount` instances of
the set of elements are executed and the value of the internal counter
`InstanceID` advances for each iteration. `InstanceID` is an internal 32-bit
integer counter that may be read by a vertex shader as `?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstanced.xhtml)

# `drawElementsInstancedBaseInstance`

```erlang
-spec drawElementsInstancedBaseInstance(Mode, Count, Type, Indices, Instancecount, Baseinstance) -> ok
                                           when
                                               Mode :: enum(),
                                               Count :: i(),
                                               Type :: enum(),
                                               Indices :: offset() | mem(),
                                               Instancecount :: i(),
                                               Baseinstance :: i().
```

[`gl:drawElementsInstancedBaseInstance/6`](`drawElementsInstancedBaseInstance/6`)
behaves identically to [`gl:drawElements/4`](`drawElements/4`) except that
`Instancecount` instances of the set of elements are executed and the value of
the internal counter `InstanceID` advances for each iteration. `InstanceID` is
an internal 32-bit integer counter that may be read by a vertex shader as
`?gl_InstanceID`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseInstance.xhtml)

# `drawElementsInstancedBaseVertex`

```erlang
-spec drawElementsInstancedBaseVertex(Mode, Count, Type, Indices, Instancecount, Basevertex) -> ok
                                         when
                                             Mode :: enum(),
                                             Count :: i(),
                                             Type :: enum(),
                                             Indices :: offset() | mem(),
                                             Instancecount :: i(),
                                             Basevertex :: i().
```

[`gl:drawElementsInstancedBaseVertex/6`](`drawElementsInstancedBaseVertex/6`)
behaves identically to [`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`)
except that the `i`th element transferred by the corresponding draw call will be
taken from element `Indices`\[i] + `Basevertex` of each enabled array. If the
resulting value is larger than the maximum value representable by `Type`, it is
as if the calculation were upconverted to 32-bit unsigned integers (with
wrapping on overflow conditions). The operation is undefined if the sum would be
negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertex.xhtml)

# `drawElementsInstancedBaseVertexBaseInstance`

```erlang
-spec drawElementsInstancedBaseVertexBaseInstance(Mode, Count, Type, Indices, Instancecount, Basevertex,
                                                  Baseinstance) ->
                                                     ok
                                                     when
                                                         Mode :: enum(),
                                                         Count :: i(),
                                                         Type :: enum(),
                                                         Indices :: offset() | mem(),
                                                         Instancecount :: i(),
                                                         Basevertex :: i(),
                                                         Baseinstance :: i().
```

[`gl:drawElementsInstancedBaseVertexBaseInstance/7`](`drawElementsInstancedBaseVertexBaseInstance/7`)
behaves identically to [`gl:drawElementsInstanced/5`](`drawElementsInstanced/5`)
except that the `i`th element transferred by the corresponding draw call will be
taken from element `Indices`\[i] + `Basevertex` of each enabled array. If the
resulting value is larger than the maximum value representable by `Type`, it is
as if the calculation were upconverted to 32-bit unsigned integers (with
wrapping on overflow conditions). The operation is undefined if the sum would be
negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml)

# `drawPixels`

```erlang
-spec drawPixels(Width :: i(),
                 Height :: i(),
                 Format :: enum(),
                 Type :: enum(),
                 Pixels :: offset() | mem()) ->
                    ok.
```

[`gl:drawPixels/5`](`drawPixels/5`) reads pixel data from memory and writes it
into the frame buffer relative to the current raster position, provided that the
raster position is valid. Use [`gl:rasterPos()`](`rasterPos2d/2`) or
[`gl:windowPos()`](`windowPos2d/2`) to set the current raster position; use
[`gl:get()`](`getBooleanv/1`) with argument `?GL_CURRENT_RASTER_POSITION_VALID`
to determine if the specified raster position is valid, and
[`gl:get()`](`getBooleanv/1`) with argument `?GL_CURRENT_RASTER_POSITION` to
query the raster position.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glDrawPixels.xml)

# `drawRangeElements`

```erlang
-spec drawRangeElements(Mode, Start, End, Count, Type, Indices) -> ok
                           when
                               Mode :: enum(),
                               Start :: i(),
                               End :: i(),
                               Count :: i(),
                               Type :: enum(),
                               Indices :: offset() | mem().
```

[`gl:drawRangeElements/6`](`drawRangeElements/6`) is a restricted form of
[`gl:drawElements/4`](`drawElements/4`). `Mode`, and `Count` match the
corresponding arguments to [`gl:drawElements/4`](`drawElements/4`), with the
additional constraint that all values in the arrays `Count` must lie between
`Start` and `End`, inclusive.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElements.xhtml)

# `drawRangeElementsBaseVertex`

```erlang
-spec drawRangeElementsBaseVertex(Mode, Start, End, Count, Type, Indices, Basevertex) -> ok
                                     when
                                         Mode :: enum(),
                                         Start :: i(),
                                         End :: i(),
                                         Count :: i(),
                                         Type :: enum(),
                                         Indices :: offset() | mem(),
                                         Basevertex :: i().
```

[`gl:drawRangeElementsBaseVertex/7`](`drawRangeElementsBaseVertex/7`) is a
restricted form of [`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`).
`Mode`, `Count` and `Basevertex` match the corresponding arguments to
[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`), with the additional
constraint that all values in the array `Indices` must lie between `Start` and
`End`, inclusive, prior to adding `Basevertex`. Index values lying outside the
range [`Start`, `End`] are treated in the same way as
[`gl:drawElementsBaseVertex/5`](`drawElementsBaseVertex/5`). The `i`th element
transferred by the corresponding draw call will be taken from element
`Indices`\[i] + `Basevertex` of each enabled array. If the resulting value is
larger than the maximum value representable by `Type`, it is as if the
calculation were upconverted to 32-bit unsigned integers (with wrapping on
overflow conditions). The operation is undefined if the sum would be negative.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawRangeElementsBaseVertex.xhtml)

# `drawTransformFeedback`

```erlang
-spec drawTransformFeedback(Mode :: enum(), Id :: i()) -> ok.
```

[`gl:drawTransformFeedback/2`](`drawTransformFeedback/2`) draws primitives of a
type specified by `Mode` using a count retrieved from the transform feedback
specified by `Id`. Calling
[`gl:drawTransformFeedback/2`](`drawTransformFeedback/2`) is equivalent to
calling [`gl:drawArrays/3`](`drawArrays/3`) with `Mode` as specified, `First`
set to zero, and `Count` set to the number of vertices captured on vertex stream
zero the last time transform feedback was active on the transform feedback
object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedback.xhtml)

# `drawTransformFeedbackInstanced`

```erlang
-spec drawTransformFeedbackInstanced(Mode :: enum(), Id :: i(), Instancecount :: i()) -> ok.
```

[`gl:drawTransformFeedbackInstanced/3`](`drawTransformFeedbackInstanced/3`)
draws multiple copies of a range of primitives of a type specified by `Mode`
using a count retrieved from the transform feedback stream specified by `Stream`
of the transform feedback object specified by `Id`. Calling
[`gl:drawTransformFeedbackInstanced/3`](`drawTransformFeedbackInstanced/3`) is
equivalent to calling [`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`) with
`Mode` and `Instancecount` as specified, `First` set to zero, and `Count` set to
the number of vertices captured on vertex stream zero the last time transform
feedback was active on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackInstanced.xhtml)

# `drawTransformFeedbackStream`

```erlang
-spec drawTransformFeedbackStream(Mode :: enum(), Id :: i(), Stream :: i()) -> ok.
```

[`gl:drawTransformFeedbackStream/3`](`drawTransformFeedbackStream/3`) draws
primitives of a type specified by `Mode` using a count retrieved from the
transform feedback stream specified by `Stream` of the transform feedback object
specified by `Id`. Calling
[`gl:drawTransformFeedbackStream/3`](`drawTransformFeedbackStream/3`) is
equivalent to calling [`gl:drawArrays/3`](`drawArrays/3`) with `Mode` as
specified, `First` set to zero, and `Count` set to the number of vertices
captured on vertex stream `Stream` the last time transform feedback was active
on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackStream.xhtml)

# `drawTransformFeedbackStreamInstanced`

```erlang
-spec drawTransformFeedbackStreamInstanced(Mode :: enum(),
                                           Id :: i(),
                                           Stream :: i(),
                                           Instancecount :: i()) ->
                                              ok.
```

[`gl:drawTransformFeedbackStreamInstanced/4`](`drawTransformFeedbackStreamInstanced/4`)
draws multiple copies of a range of primitives of a type specified by `Mode`
using a count retrieved from the transform feedback stream specified by `Stream`
of the transform feedback object specified by `Id`. Calling
[`gl:drawTransformFeedbackStreamInstanced/4`](`drawTransformFeedbackStreamInstanced/4`)
is equivalent to calling [`gl:drawArraysInstanced/4`](`drawArraysInstanced/4`)
with `Mode` and `Instancecount` as specified, `First` set to zero, and `Count`
set to the number of vertices captured on vertex stream `Stream` the last time
transform feedback was active on the transform feedback object named by `Id`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawTransformFeedbackStreamInstanced.xhtml)

# `edgeFlag`

```erlang
-spec edgeFlag(Flag :: 0 | 1) -> ok.
```

# `edgeFlagPointer`

```erlang
-spec edgeFlagPointer(Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:edgeFlagPointer/2`](`edgeFlagPointer/2`) specifies the location and data
format of an array of boolean edge flags to use when rendering. `Stride`
specifies the byte stride from one edge flag to the next, allowing vertices and
attributes to be packed into a single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlagPointer.xml)

# `edgeFlagv`

```erlang
-spec edgeFlagv({Flag :: 0 | 1}) -> ok.
```

Each vertex of a polygon, separate triangle, or separate quadrilateral specified
between a [`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pair is
marked as the start of either a boundary or nonboundary edge. If the current
edge flag is true when the vertex is specified, the vertex is marked as the
start of a boundary edge. Otherwise, the vertex is marked as the start of a
nonboundary edge. [`gl:edgeFlag/1`](`edgeFlag/1`) sets the edge flag bit to
`?GL_TRUE` if `Flag` is `?GL_TRUE` and to `?GL_FALSE` otherwise.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEdgeFlag.xml)

# `enable`

```erlang
-spec enable(Cap :: enum()) -> ok.
```

# `enableClientState`

```erlang
-spec enableClientState(Cap :: enum()) -> ok.
```

[`gl:enableClientState/1`](`enableClientState/1`) and
[`gl:disableClientState/1`](`enableClientState/1`) enable or disable individual
client-side capabilities. By default, all client-side capabilities are disabled.
Both [`gl:enableClientState/1`](`enableClientState/1`) and
[`gl:disableClientState/1`](`enableClientState/1`) take a single argument,
`Cap`, which can assume one of the following values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEnableClientState.xml)

# `enablei`

```erlang
-spec enablei(Target :: enum(), Index :: i()) -> ok.
```

[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`) enable and disable
various capabilities. Use [`gl:isEnabled/1`](`isEnabled/1`) or
[`gl:get()`](`getBooleanv/1`) to determine the current setting of any
capability. The initial value for each capability with the exception of
`?GL_DITHER` and `?GL_MULTISAMPLE` is `?GL_FALSE`. The initial value for
`?GL_DITHER` and `?GL_MULTISAMPLE` is `?GL_TRUE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnable.xhtml)

# `enableVertexArrayAttrib`

```erlang
-spec enableVertexArrayAttrib(Vaobj :: i(), Index :: i()) -> ok.
```

# `enableVertexAttribArray`

```erlang
-spec enableVertexAttribArray(Index :: i()) -> ok.
```

[`gl:enableVertexAttribArray/1`](`enableVertexAttribArray/1`) and
[`gl:enableVertexArrayAttrib/2`](`disableVertexAttribArray/1`) enable the
generic vertex attribute array specified by `Index`.
[`gl:enableVertexAttribArray/1`](`enableVertexAttribArray/1`) uses currently
bound vertex array object for the operation, whereas
[`gl:enableVertexArrayAttrib/2`](`disableVertexAttribArray/1`) updates state of
the vertex array object with ID `Vaobj`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml)

# `end`

```erlang
-spec 'end'() -> ok.
```

[`gl:'begin'/1`](`'begin'/1`) and [`gl:'end'/0`](`'begin'/1`) delimit the
vertices that define a primitive or a group of like primitives.
[`gl:'begin'/1`](`'begin'/1`) accepts a single argument that specifies in which
of ten ways the vertices are interpreted. Taking n as an integer count starting
at one, and N as the total number of vertices specified, the interpretations are
as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glBegin.xml)

# `endConditionalRender`

```erlang
-spec endConditionalRender() -> ok.
```

Conditional rendering is started using
[`gl:beginConditionalRender/2`](`beginConditionalRender/2`) and ended using
[`gl:endConditionalRender/0`](`beginConditionalRender/2`). During conditional
rendering, all vertex array commands, as well as [`gl:clear/1`](`clear/1`) and
[`gl:clearBuffer()`](`clearBufferiv/3`) have no effect if the
(`?GL_SAMPLES_PASSED`) result of the query object `Id` is zero, or if the
(`?GL_ANY_SAMPLES_PASSED`) result is `?GL_FALSE`. The results of commands
setting the current vertex state, such as
[`gl:vertexAttrib()`](`vertexAttrib1d/2`) are undefined. If the
(`?GL_SAMPLES_PASSED`) result is non-zero or if the (`?GL_ANY_SAMPLES_PASSED`)
result is `?GL_TRUE`, such commands are not discarded. The `Id` parameter to
[`gl:beginConditionalRender/2`](`beginConditionalRender/2`) must be the name of
a query object previously returned from a call to
[`gl:genQueries/1`](`genQueries/1`). `Mode` specifies how the results of the
query object are to be interpreted. If `Mode` is `?GL_QUERY_WAIT`, the GL waits
for the results of the query to be available and then uses the results to
determine if subsequent rendering commands are discarded. If `Mode` is
`?GL_QUERY_NO_WAIT`, the GL may choose to unconditionally execute the subsequent
rendering commands without waiting for the query to complete.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginConditionalRender.xhtml)

# `endList`

```erlang
-spec endList() -> ok.
```

# `endQuery`

```erlang
-spec endQuery(Target :: enum()) -> ok.
```

[`gl:beginQuery/2`](`beginQuery/2`) and [`gl:endQuery/1`](`beginQuery/2`)
delimit the boundaries of a query object. `Query` must be a name previously
returned from a call to [`gl:genQueries/1`](`genQueries/1`). If a query object
with name `Id` does not yet exist it is created with the type determined by
`Target`. `Target` must be one of `?GL_SAMPLES_PASSED`,
`?GL_ANY_SAMPLES_PASSED`, `?GL_PRIMITIVES_GENERATED`,
`?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN`, or `?GL_TIME_ELAPSED`. The behavior
of the query object depends on its type and is as follows.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQuery.xhtml)

# `endQueryIndexed`

```erlang
-spec endQueryIndexed(Target :: enum(), Index :: i()) -> ok.
```

[`gl:beginQueryIndexed/3`](`beginQueryIndexed/3`) and
[`gl:endQueryIndexed/2`](`beginQueryIndexed/3`) delimit the boundaries of a
query object. `Query` must be a name previously returned from a call to
[`gl:genQueries/1`](`genQueries/1`). If a query object with name `Id` does not
yet exist it is created with the type determined by `Target`. `Target` must be
one of `?GL_SAMPLES_PASSED`, `?GL_ANY_SAMPLES_PASSED`,
`?GL_PRIMITIVES_GENERATED`, `?GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN`, or
`?GL_TIME_ELAPSED`. The behavior of the query object depends on its type and is
as follows.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQueryIndexed.xhtml)

# `endTransformFeedback`

```erlang
-spec endTransformFeedback() -> ok.
```

Transform feedback mode captures the values of varying variables written by the
vertex shader (or, if active, the geometry shader). Transform feedback is said
to be active after a call to
[`gl:beginTransformFeedback/1`](`beginTransformFeedback/1`) until a subsequent
call to [`gl:endTransformFeedback/0`](`beginTransformFeedback/1`). Transform
feedback commands must be paired.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginTransformFeedback.xhtml)

# `evalCoord1d`

```erlang
-spec evalCoord1d(U :: f()) -> ok.
```

# `evalCoord1dv`

```erlang
-spec evalCoord1dv({U :: f()}) -> ok.
```

# `evalCoord1f`

```erlang
-spec evalCoord1f(U :: f()) -> ok.
```

# `evalCoord1fv`

```erlang
-spec evalCoord1fv({U :: f()}) -> ok.
```

# `evalCoord2d`

```erlang
-spec evalCoord2d(U :: f(), V :: f()) -> ok.
```

# `evalCoord2dv`

```erlang
-spec evalCoord2dv({U :: f(), V :: f()}) -> ok.
```

# `evalCoord2f`

```erlang
-spec evalCoord2f(U :: f(), V :: f()) -> ok.
```

# `evalCoord2fv`

```erlang
-spec evalCoord2fv({U :: f(), V :: f()}) -> ok.
```

[`gl:evalCoord1()`](`evalCoord1d/1`) evaluates enabled one-dimensional maps at
argument `U`. [`gl:evalCoord2()`](`evalCoord1d/1`) does the same for
two-dimensional maps using two domain values, `U` and `V`. To define a map, call
`glMap1` and `glMap2`; to enable and disable it, call
[`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalCoord.xml)

# `evalMesh1`

```erlang
-spec evalMesh1(Mode :: enum(), I1 :: i(), I2 :: i()) -> ok.
```

# `evalMesh2`

```erlang
-spec evalMesh2(Mode :: enum(), I1 :: i(), I2 :: i(), J1 :: i(), J2 :: i()) -> ok.
```

[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used in
tandem to efficiently generate and evaluate a series of evenly-spaced map domain
values. [`gl:evalMesh()`](`evalMesh1/3`) steps through the integer domain of a
one- or two-dimensional grid, whose range is the domain of the evaluation maps
specified by `glMap1` and `glMap2`. `Mode` determines whether the resulting
vertices are connected as points, lines, or filled polygons.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalMesh.xml)

# `evalPoint1`

```erlang
-spec evalPoint1(I :: i()) -> ok.
```

# `evalPoint2`

```erlang
-spec evalPoint2(I :: i(), J :: i()) -> ok.
```

[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used in
tandem to efficiently generate and evaluate a series of evenly spaced map domain
values. [`gl:evalPoint()`](`evalPoint1/1`) can be used to evaluate a single grid
point in the same gridspace that is traversed by
[`gl:evalMesh()`](`evalMesh1/3`). Calling [`gl:evalPoint1/1`](`evalPoint1/1`) is
equivalent to calling glEvalCoord1( i.ð u+u 1 ); where ð u=(u 2-u 1)/n

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glEvalPoint.xml)

# `feedbackBuffer`

```erlang
-spec feedbackBuffer(Size :: i(), Type :: enum(), Buffer :: mem()) -> ok.
```

The [`gl:feedbackBuffer/3`](`feedbackBuffer/3`) function controls feedback.
Feedback, like selection, is a GL mode. The mode is selected by calling
[`gl:renderMode/1`](`renderMode/1`) with `?GL_FEEDBACK`. When the GL is in
feedback mode, no pixels are produced by rasterization. Instead, information
about primitives that would have been rasterized is fed back to the application
using the GL.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFeedbackBuffer.xml)

# `fenceSync`

```erlang
-spec fenceSync(Condition :: enum(), Flags :: i()) -> i().
```

[`gl:fenceSync/2`](`fenceSync/2`) creates a new fence sync object, inserts a
fence command into the GL command stream and associates it with that sync
object, and returns a non-zero name corresponding to the sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFenceSync.xhtml)

# `finish`

```erlang
-spec finish() -> ok.
```

[`gl:finish/0`](`finish/0`) does not return until the effects of all previously
called GL commands are complete. Such effects include all changes to GL state,
all changes to connection state, and all changes to the frame buffer contents.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFinish.xhtml)

# `flush`

```erlang
-spec flush() -> ok.
```

Different GL implementations buffer commands in several different locations,
including network buffers and the graphics accelerator itself.
[`gl:flush/0`](`flush/0`) empties all of these buffers, causing all issued
commands to be executed as quickly as they are accepted by the actual rendering
engine. Though this execution may not be completed in any particular time
period, it does complete in finite time.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlush.xhtml)

# `flushMappedBufferRange`

```erlang
-spec flushMappedBufferRange(Target :: enum(), Offset :: i(), Length :: i()) -> ok.
```

# `flushMappedNamedBufferRange`

```erlang
-spec flushMappedNamedBufferRange(Buffer :: i(), Offset :: i(), Length :: i()) -> ok.
```

[`gl:flushMappedBufferRange/3`](`flushMappedBufferRange/3`) indicates that
modifications have been made to a range of a mapped buffer object. The buffer
object must previously have been mapped with the `?GL_MAP_FLUSH_EXPLICIT_BIT`
flag.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFlushMappedBufferRange.xhtml)

# `fogCoordd`

```erlang
-spec fogCoordd(Coord :: f()) -> ok.
```

# `fogCoorddv`

```erlang
-spec fogCoorddv({Coord :: f()}) -> ok.
```

# `fogCoordf`

```erlang
-spec fogCoordf(Coord :: f()) -> ok.
```

# `fogCoordfv`

```erlang
-spec fogCoordfv({Coord :: f()}) -> ok.
```

[`gl:fogCoord()`](`fogCoordf/1`) specifies the fog coordinate that is associated
with each vertex and the current raster position. The value specified is
interpolated and used in computing the fog color (see [`gl:fog()`](`fogf/2`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoord.xml)

# `fogCoordPointer`

```erlang
-spec fogCoordPointer(Type :: enum(), Stride :: i(), Pointer :: offset() | mem()) -> ok.
```

[`gl:fogCoordPointer/3`](`fogCoordPointer/3`) specifies the location and data
format of an array of fog coordinates to use when rendering. `Type` specifies
the data type of each fog coordinate, and `Stride` specifies the byte stride
from one fog coordinate to the next, allowing vertices and attributes to be
packed into a single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFogCoordPointer.xml)

# `fogf`

```erlang
-spec fogf(Pname :: enum(), Param :: f()) -> ok.
```

# `fogfv`

```erlang
-spec fogfv(Pname :: enum(), Params :: tuple()) -> ok.
```

# `fogi`

```erlang
-spec fogi(Pname :: enum(), Param :: i()) -> ok.
```

# `fogiv`

```erlang
-spec fogiv(Pname :: enum(), Params :: tuple()) -> ok.
```

Fog is initially disabled. While enabled, fog affects rasterized geometry,
bitmaps, and pixel blocks, but not buffer clear operations. To enable and
disable fog, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_FOG`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFog.xml)

# `framebufferParameteri`

```erlang
-spec framebufferParameteri(Target :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

[`gl:framebufferParameteri/3`](`framebufferParameteri/3`) and
`glNamedFramebufferParameteri` modify the value of the parameter named `Pname`
in the specified framebuffer object. There are no modifiable parameters of the
default draw and read framebuffer, so they are not valid targets of these
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferParameteri.xhtml)

# `framebufferRenderbuffer`

```erlang
-spec framebufferRenderbuffer(Target, Attachment, Renderbuffertarget, Renderbuffer) -> ok
                                 when
                                     Target :: enum(),
                                     Attachment :: enum(),
                                     Renderbuffertarget :: enum(),
                                     Renderbuffer :: i().
```

[`gl:framebufferRenderbuffer/4`](`framebufferRenderbuffer/4`) and
`glNamedFramebufferRenderbuffer` attaches a renderbuffer as one of the logical
buffers of the specified framebuffer object. Renderbuffers cannot be attached to
the default draw and read framebuffer, so they are not valid targets of these
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferRenderbuffer.xhtml)

# `framebufferTexture1D`

```erlang
-spec framebufferTexture1D(Target :: enum(),
                           Attachment :: enum(),
                           Textarget :: enum(),
                           Texture :: i(),
                           Level :: i()) ->
                              ok.
```

# `framebufferTexture2D`

```erlang
-spec framebufferTexture2D(Target :: enum(),
                           Attachment :: enum(),
                           Textarget :: enum(),
                           Texture :: i(),
                           Level :: i()) ->
                              ok.
```

# `framebufferTexture3D`

```erlang
-spec framebufferTexture3D(Target, Attachment, Textarget, Texture, Level, Zoffset) -> ok
                              when
                                  Target :: enum(),
                                  Attachment :: enum(),
                                  Textarget :: enum(),
                                  Texture :: i(),
                                  Level :: i(),
                                  Zoffset :: i().
```

# `framebufferTexture`

```erlang
-spec framebufferTexture(Target :: enum(), Attachment :: enum(), Texture :: i(), Level :: i()) -> ok.
```

# `framebufferTextureFaceARB`

```erlang
-spec framebufferTextureFaceARB(Target :: enum(),
                                Attachment :: enum(),
                                Texture :: i(),
                                Level :: i(),
                                Face :: enum()) ->
                                   ok.
```

# `framebufferTextureLayer`

```erlang
-spec framebufferTextureLayer(Target :: enum(),
                              Attachment :: enum(),
                              Texture :: i(),
                              Level :: i(),
                              Layer :: i()) ->
                                 ok.
```

These commands attach a selected mipmap level or image of a texture object as
one of the logical buffers of the specified framebuffer object. Textures cannot
be attached to the default draw and read framebuffer, so they are not valid
targets of these commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferTexture.xhtml)

# `frontFace`

```erlang
-spec frontFace(Mode :: enum()) -> ok.
```

In a scene composed entirely of opaque closed surfaces, back-facing polygons are
never visible. Eliminating these invisible polygons has the obvious benefit of
speeding up the rendering of the image. To enable and disable elimination of
back-facing polygons, call [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) with argument `?GL_CULL_FACE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFrontFace.xhtml)

# `frustum`

```erlang
-spec frustum(Left :: f(), Right :: f(), Bottom :: f(), Top :: f(), Near_val :: f(), Far_val :: f()) ->
                 ok.
```

[`gl:frustum/6`](`frustum/6`) describes a perspective matrix that produces a
perspective projection. The current matrix (see
[`gl:matrixMode/1`](`matrixMode/1`)) is multiplied by this matrix and the result
replaces the current matrix, as if [`gl:multMatrix()`](`multMatrixd/1`) were
called with the following matrix as its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glFrustum.xml)

# `genBuffers`

```erlang
-spec genBuffers(N :: i()) -> [i()].
```

[`gl:genBuffers/1`](`genBuffers/1`) returns `N` buffer object names in
`Buffers`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genBuffers/1`](`genBuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenBuffers.xhtml)

# `generateMipmap`

```erlang
-spec generateMipmap(Target :: enum()) -> ok.
```

# `generateTextureMipmap`

```erlang
-spec generateTextureMipmap(Texture :: i()) -> ok.
```

[`gl:generateMipmap/1`](`generateMipmap/1`) and
[`gl:generateTextureMipmap/1`](`generateMipmap/1`) generates mipmaps for the
specified texture object. For [`gl:generateMipmap/1`](`generateMipmap/1`), the
texture object that is bound to `Target`. For
[`gl:generateTextureMipmap/1`](`generateMipmap/1`), `Texture` is the name of the
texture object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenerateMipmap.xhtml)

# `genFramebuffers`

```erlang
-spec genFramebuffers(N :: i()) -> [i()].
```

[`gl:genFramebuffers/1`](`genFramebuffers/1`) returns `N` framebuffer object
names in `Ids`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genFramebuffers/1`](`genFramebuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenFramebuffers.xhtml)

# `genLists`

```erlang
-spec genLists(Range :: i()) -> i().
```

[`gl:genLists/1`](`genLists/1`) has one argument, `Range`. It returns an integer
`n` such that `Range` contiguous empty display lists, named n, n+1, ...,
n+range-1, are created. If `Range` is 0, if there is no group of `Range`
contiguous names available, or if any error is generated, no display lists are
generated, and 0 is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenLists.xml)

# `genProgramPipelines`

```erlang
-spec genProgramPipelines(N :: i()) -> [i()].
```

[`gl:genProgramPipelines/1`](`genProgramPipelines/1`) returns `N` previously
unused program pipeline object names in `Pipelines`. These names are marked as
used, for the purposes of [`gl:genProgramPipelines/1`](`genProgramPipelines/1`)
only, but they acquire program pipeline state only when they are first bound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenProgramPipelines.xhtml)

# `genQueries`

```erlang
-spec genQueries(N :: i()) -> [i()].
```

[`gl:genQueries/1`](`genQueries/1`) returns `N` query object names in `Ids`.
There is no guarantee that the names form a contiguous set of integers; however,
it is guaranteed that none of the returned names was in use immediately before
the call to [`gl:genQueries/1`](`genQueries/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenQueries.xhtml)

# `genRenderbuffers`

```erlang
-spec genRenderbuffers(N :: i()) -> [i()].
```

[`gl:genRenderbuffers/1`](`genRenderbuffers/1`) returns `N` renderbuffer object
names in `Renderbuffers`. There is no guarantee that the names form a contiguous
set of integers; however, it is guaranteed that none of the returned names was
in use immediately before the call to
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenRenderbuffers.xhtml)

# `genSamplers`

```erlang
-spec genSamplers(Count :: i()) -> [i()].
```

[`gl:genSamplers/1`](`genSamplers/1`) returns `N` sampler object names in
`Samplers`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genSamplers/1`](`genSamplers/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenSamplers.xhtml)

# `genTextures`

```erlang
-spec genTextures(N :: i()) -> [i()].
```

[`gl:genTextures/1`](`genTextures/1`) returns `N` texture names in `Textures`.
There is no guarantee that the names form a contiguous set of integers; however,
it is guaranteed that none of the returned names was in use immediately before
the call to [`gl:genTextures/1`](`genTextures/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTextures.xhtml)

# `genTransformFeedbacks`

```erlang
-spec genTransformFeedbacks(N :: i()) -> [i()].
```

[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`) returns `N` previously
unused transform feedback object names in `Ids`. These names are marked as used,
for the purposes of [`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`)
only, but they acquire transform feedback state only when they are first bound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTransformFeedbacks.xhtml)

# `genVertexArrays`

```erlang
-spec genVertexArrays(N :: i()) -> [i()].
```

[`gl:genVertexArrays/1`](`genVertexArrays/1`) returns `N` vertex array object
names in `Arrays`. There is no guarantee that the names form a contiguous set of
integers; however, it is guaranteed that none of the returned names was in use
immediately before the call to [`gl:genVertexArrays/1`](`genVertexArrays/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenVertexArrays.xhtml)

# `getActiveAttrib`

```erlang
-spec getActiveAttrib(Program :: i(), Index :: i(), BufSize :: i()) ->
                         {Size :: i(), Type :: enum(), Name :: string()}.
```

[`gl:getActiveAttrib/3`](`getActiveAttrib/3`) returns information about an
active attribute variable in the program object specified by `Program`. The
number of active attributes can be obtained by calling
[`gl:getProgram()`](`getProgramiv/2`) with the value `?GL_ACTIVE_ATTRIBUTES`. A
value of 0 for `Index` selects the first active attribute variable. Permissible
values for `Index` range from zero to the number of active attribute variables
minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveAttrib.xhtml)

# `getActiveSubroutineName`

```erlang
-spec getActiveSubroutineName(Program :: i(), Shadertype :: enum(), Index :: i(), Bufsize :: i()) ->
                                 string().
```

[`gl:getActiveSubroutineName/4`](`getActiveSubroutineName/4`) queries the name
of an active shader subroutine uniform from the program object given in
`Program`. `Index` specifies the index of the shader subroutine uniform within
the shader stage given by `Stage`, and must between zero and the value of
`?GL_ACTIVE_SUBROUTINES` minus one for the shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineName.xhtml)

# `getActiveSubroutineUniformName`

```erlang
-spec getActiveSubroutineUniformName(Program :: i(), Shadertype :: enum(), Index :: i(), Bufsize :: i()) ->
                                        string().
```

[`gl:getActiveSubroutineUniformName/4`](`getActiveSubroutineUniformName/4`)
retrieves the name of an active shader subroutine uniform. `Program` contains
the name of the program containing the uniform. `Shadertype` specifies the stage
for which the uniform location, given by `Index`, is valid. `Index` must be
between zero and the value of `?GL_ACTIVE_SUBROUTINE_UNIFORMS` minus one for the
shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveSubroutineUniformName.xhtml)

# `getActiveUniform`

```erlang
-spec getActiveUniform(Program :: i(), Index :: i(), BufSize :: i()) ->
                          {Size :: i(), Type :: enum(), Name :: string()}.
```

[`gl:getActiveUniform/3`](`getActiveUniform/3`) returns information about an
active uniform variable in the program object specified by `Program`. The number
of active uniform variables can be obtained by calling
[`gl:getProgram()`](`getProgramiv/2`) with the value `?GL_ACTIVE_UNIFORMS`. A
value of 0 for `Index` selects the first active uniform variable. Permissible
values for `Index` range from zero to the number of active uniform variables
minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniform.xhtml)

# `getActiveUniformBlockiv`

```erlang
-spec getActiveUniformBlockiv(Program :: i(),
                              UniformBlockIndex :: i(),
                              Pname :: enum(),
                              Params :: mem()) ->
                                 ok.
```

[`gl:getActiveUniformBlockiv/4`](`getActiveUniformBlockiv/4`) retrieves
information about an active uniform block within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlock.xhtml)

# `getActiveUniformBlockName`

```erlang
-spec getActiveUniformBlockName(Program :: i(), UniformBlockIndex :: i(), BufSize :: i()) -> string().
```

[`gl:getActiveUniformBlockName/3`](`getActiveUniformBlockName/3`) retrieves the
name of the active uniform block at `UniformBlockIndex` within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformBlockName.xhtml)

# `getActiveUniformName`

```erlang
-spec getActiveUniformName(Program :: i(), UniformIndex :: i(), BufSize :: i()) -> string().
```

[`gl:getActiveUniformName/3`](`getActiveUniformName/3`) returns the name of the
active uniform at `UniformIndex` within `Program`. If `UniformName` is not NULL,
up to `BufSize` characters (including a nul-terminator) will be written into the
array whose address is specified by `UniformName`. If `Length` is not NULL, the
number of characters that were (or would have been) written into `UniformName`
(not including the nul-terminator) will be placed in the variable whose address
is specified in `Length`. If `Length` is NULL, no length is returned. The length
of the longest uniform name in `Program` is given by the value of
`?GL_ACTIVE_UNIFORM_MAX_LENGTH`, which can be queried with
[`gl:getProgram()`](`getProgramiv/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformName.xhtml)

# `getActiveUniformsiv`

```erlang
-spec getActiveUniformsiv(Program :: i(), UniformIndices :: [i()], Pname :: enum()) -> [i()].
```

[`gl:getActiveUniformsiv/3`](`getActiveUniformsiv/3`) queries the value of the
parameter named `Pname` for each of the uniforms within `Program` whose indices
are specified in the array of `UniformCount` unsigned integers `UniformIndices`.
Upon success, the value of the parameter for each uniform is written into the
corresponding entry in the array whose address is given in `Params`. If an error
is generated, nothing is written into `Params`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniformsiv.xhtml)

# `getAttachedShaders`

```erlang
-spec getAttachedShaders(Program :: i(), MaxCount :: i()) -> [i()].
```

[`gl:getAttachedShaders/2`](`getAttachedShaders/2`) returns the names of the
shader objects attached to `Program`. The names of shader objects that are
attached to `Program` will be returned in `Shaders.` The actual number of shader
names written into `Shaders` is returned in `Count.` If no shader objects are
attached to `Program`, `Count` is set to 0. The maximum number of shader names
that may be returned in `Shaders` is specified by `MaxCount`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttachedShaders.xhtml)

# `getAttribLocation`

```erlang
-spec getAttribLocation(Program :: i(), Name :: string()) -> i().
```

[`gl:getAttribLocation/2`](`getAttribLocation/2`) queries the previously linked
program object specified by `Program` for the attribute variable specified by
`Name` and returns the index of the generic vertex attribute that is bound to
that attribute variable. If `Name` is a matrix attribute variable, the index of
the first column of the matrix is returned. If the named attribute variable is
not an active attribute in the specified program object or if `Name` starts with
the reserved prefix "gl\_", a value of -1 is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttribLocation.xhtml)

# `getBooleani_v`

```erlang
-spec getBooleani_v(Target :: enum(), Index :: i()) -> [0 | 1].
```

# `getBooleanv`

```erlang
-spec getBooleanv(Pname :: enum()) -> [0 | 1].
```

# `getBufferParameteri64v`

```erlang
-spec getBufferParameteri64v(Target :: enum(), Pname :: enum()) -> [i()].
```

# `getBufferParameteriv`

```erlang
-spec getBufferParameteriv(Target :: enum(), Pname :: enum()) -> i().
```

[`gl:getBufferParameteriv/2`](`getBufferParameteriv/2`) returns in `Data` a
selected parameter of the buffer object specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetBufferParameteriv.xml)

# `getBufferParameterivARB`

```erlang
-spec getBufferParameterivARB(Target :: enum(), Pname :: enum()) -> [i()].
```

These functions return in `Data` a selected parameter of the specified buffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetBufferParameter.xhtml)

# `getBufferSubData`

```erlang
-spec getBufferSubData(Target :: enum(), Offset :: i(), Size :: i(), Data :: mem()) -> ok.
```

[`gl:getBufferSubData/4`](`getBufferSubData/4`) and `glGetNamedBufferSubData`
return some or all of the data contents of the data store of the specified
buffer object. Data starting at byte offset `Offset` and extending for `Size`
bytes is copied from the buffer object's data store to the memory pointed to by
`Data`. An error is thrown if the buffer object is currently mapped, or if
`Offset` and `Size` together define a range beyond the bounds of the buffer
object's data store.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetBufferSubData.xhtml)

# `getClipPlane`

```erlang
-spec getClipPlane(Plane :: enum()) -> {f(), f(), f(), f()}.
```

[`gl:getClipPlane/1`](`getClipPlane/1`) returns in `Equation` the four
coefficients of the plane equation for `Plane`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetClipPlane.xml)

# `getColorTable`

```erlang
-spec getColorTable(Target :: enum(), Format :: enum(), Type :: enum(), Table :: mem()) -> ok.
```

[`gl:getColorTable/4`](`getColorTable/4`) returns in `Table` the contents of the
color table specified by `Target`. No pixel transfer operations are performed,
but pixel storage modes that are applicable to
[`gl:readPixels/7`](`readPixels/7`) are performed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTable.xml)

# `getColorTableParameterfv`

```erlang
-spec getColorTableParameterfv(Target :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getColorTableParameteriv`

```erlang
-spec getColorTableParameteriv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

Returns parameters specific to color table `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetColorTableParameter.xml)

# `getCompressedTexImage`

```erlang
-spec getCompressedTexImage(Target :: enum(), Lod :: i(), Img :: mem()) -> ok.
```

[`gl:getCompressedTexImage/3`](`getCompressedTexImage/3`) and
`glGetnCompressedTexImage` return the compressed texture image associated with
`Target` and `Lod` into `Pixels`. `glGetCompressedTextureImage` serves the same
purpose, but instead of taking a texture target, it takes the ID of the texture
object. `Pixels` should be an array of `BufSize` bytes for
`glGetnCompresedTexImage` and `glGetCompressedTextureImage` functions, and of
`?GL_TEXTURE_COMPRESSED_IMAGE_SIZE` bytes in case of
[`gl:getCompressedTexImage/3`](`getCompressedTexImage/3`). If the actual data
takes less space than `BufSize`, the remaining bytes will not be touched.
`Target` specifies the texture target, to which the texture the data the
function should extract the data from is bound to. `Lod` specifies the
level-of-detail number of the desired image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetCompressedTexImage.xhtml)

# `getConvolutionFilter`

```erlang
-spec getConvolutionFilter(Target :: enum(), Format :: enum(), Type :: enum(), Image :: mem()) -> ok.
```

[`gl:getConvolutionFilter/4`](`getConvolutionFilter/4`) returns the current 1D
or 2D convolution filter kernel as an image. The one- or two-dimensional image
is placed in `Image` according to the specifications in `Format` and `Type`. No
pixel transfer operations are performed on this image, but the relevant pixel
storage modes are applied.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionFilter.xml)

# `getConvolutionParameterfv`

```erlang
-spec getConvolutionParameterfv(Target :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getConvolutionParameteriv`

```erlang
-spec getConvolutionParameteriv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getConvolutionParameter()`](`getConvolutionParameterfv/2`) retrieves
convolution parameters. `Target` determines which convolution filter is queried.
`Pname` determines which parameter is returned:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetConvolutionParameter.xml)

# `getDebugMessageLog`

```erlang
-spec getDebugMessageLog(Count :: i(), BufSize :: i()) ->
                            {i(),
                             Sources :: [enum()],
                             Types :: [enum()],
                             Ids :: [i()],
                             Severities :: [enum()],
                             MessageLog :: [string()]}.
```

[`gl:getDebugMessageLog/2`](`getDebugMessageLog/2`) retrieves messages from the
debug message log. A maximum of `Count` messages are retrieved from the log. If
`Sources` is not NULL then the source of each message is written into up to
`Count` elements of the array. If `Types` is not NULL then the type of each
message is written into up to `Count` elements of the array. If `Id` is not NULL
then the identifier of each message is written into up to `Count` elements of
the array. If `Severities` is not NULL then the severity of each message is
written into up to `Count` elements of the array. If `Lengths` is not NULL then
the length of each message is written into up to `Count` elements of the array.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetDebugMessageLog.xhtml)

# `getDoublei_v`

```erlang
-spec getDoublei_v(Target :: enum(), Index :: i()) -> [f()].
```

# `getDoublev`

```erlang
-spec getDoublev(Pname :: enum()) -> [f()].
```

# `getError`

```erlang
-spec getError() -> enum().
```

[`gl:getError/0`](`getError/0`) returns the value of the error flag. Each
detectable error is assigned a numeric code and symbolic name. When an error
occurs, the error flag is set to the appropriate error code value. No other
errors are recorded until [`gl:getError/0`](`getError/0`) is called, the error
code is returned, and the flag is reset to `?GL_NO_ERROR`. If a call to
[`gl:getError/0`](`getError/0`) returns `?GL_NO_ERROR`, there has been no
detectable error since the last call to [`gl:getError/0`](`getError/0`), or
since the GL was initialized.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetError.xhtml)

# `getFloati_v`

```erlang
-spec getFloati_v(Target :: enum(), Index :: i()) -> [f()].
```

# `getFloatv`

```erlang
-spec getFloatv(Pname :: enum()) -> [f()].
```

# `getFragDataIndex`

```erlang
-spec getFragDataIndex(Program :: i(), Name :: string()) -> i().
```

[`gl:getFragDataIndex/2`](`getFragDataIndex/2`) returns the index of the
fragment color to which the variable `Name` was bound when the program object
`Program` was last linked. If `Name` is not a varying out variable of `Program`,
or if an error occurs, -1 will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataIndex.xhtml)

# `getFragDataLocation`

```erlang
-spec getFragDataLocation(Program :: i(), Name :: string()) -> i().
```

[`gl:getFragDataLocation/2`](`getFragDataLocation/2`) retrieves the assigned
color number binding for the user-defined varying out variable `Name` for
program `Program`. `Program` must have previously been linked. `Name` must be a
null-terminated string. If `Name` is not the name of an active user-defined
varying out fragment shader variable within `Program`, -1 will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFragDataLocation.xhtml)

# `getFramebufferAttachmentParameteriv`

```erlang
-spec getFramebufferAttachmentParameteriv(Target :: enum(), Attachment :: enum(), Pname :: enum()) ->
                                             i().
```

[`gl:getFramebufferAttachmentParameteriv/3`](`getFramebufferAttachmentParameteriv/3`)
and `glGetNamedFramebufferAttachmentParameteriv` return parameters of
attachments of a specified framebuffer object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFramebufferAttachmentParameter.xhtml)

# `getFramebufferParameteriv`

```erlang
-spec getFramebufferParameteriv(Target :: enum(), Pname :: enum()) -> i().
```

[`gl:getFramebufferParameteriv/2`](`getFramebufferParameteriv/2`) and
`glGetNamedFramebufferParameteriv` query parameters of a specified framebuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetFramebufferParameter.xhtml)

# `getGraphicsResetStatus`

```erlang
-spec getGraphicsResetStatus() -> enum().
```

Certain events can result in a reset of the GL context. Such a reset causes all
context state to be lost and requires the application to recreate all objects in
the affected context.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetGraphicsResetStatus.xhtml)

# `getHistogram`

```erlang
-spec getHistogram(Target :: enum(), Reset :: 0 | 1, Format :: enum(), Type :: enum(), Values :: mem()) ->
                      ok.
```

[`gl:getHistogram/5`](`getHistogram/5`) returns the current histogram table as a
one-dimensional image with the same width as the histogram. No pixel transfer
operations are performed on this image, but pixel storage modes that are
applicable to 1D images are honored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogram.xml)

# `getHistogramParameterfv`

```erlang
-spec getHistogramParameterfv(Target :: enum(), Pname :: enum()) -> {f()}.
```

# `getHistogramParameteriv`

```erlang
-spec getHistogramParameteriv(Target :: enum(), Pname :: enum()) -> {i()}.
```

[`gl:getHistogramParameter()`](`getHistogramParameterfv/2`) is used to query
parameter values for the current histogram or for a proxy. The histogram state
information may be queried by calling
[`gl:getHistogramParameter()`](`getHistogramParameterfv/2`) with a `Target` of
`?GL_HISTOGRAM` (to obtain information for the current histogram table) or
`?GL_PROXY_HISTOGRAM` (to obtain information from the most recent proxy request)
and one of the following values for the `Pname` argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetHistogramParameter.xml)

# `getInteger64i_v`

```erlang
-spec getInteger64i_v(Target :: enum(), Index :: i()) -> [i()].
```

# `getInteger64v`

```erlang
-spec getInteger64v(Pname :: enum()) -> [i()].
```

# `getIntegeri_v`

```erlang
-spec getIntegeri_v(Target :: enum(), Index :: i()) -> [i()].
```

# `getIntegerv`

```erlang
-spec getIntegerv(Pname :: enum()) -> [i()].
```

These commands return values for simple state variables in GL. `Pname` is a
symbolic constant indicating the state variable to be returned, and `Data` is a
pointer to an array of the indicated type in which to place the returned data.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGet.xhtml)

# `getInternalformati64v`

```erlang
-spec getInternalformati64v(Target :: enum(), Internalformat :: enum(), Pname :: enum(), BufSize :: i()) ->
                               [i()].
```

# `getInternalformativ`

```erlang
-spec getInternalformativ(Target :: enum(), Internalformat :: enum(), Pname :: enum(), BufSize :: i()) ->
                             [i()].
```

No documentation available.

# `getLightfv`

```erlang
-spec getLightfv(Light :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getLightiv`

```erlang
-spec getLightiv(Light :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getLight()`](`getLightfv/2`) returns in `Params` the value or values of a
light source parameter. `Light` names the light and is a symbolic name of the
form `?GL_LIGHT` i where i ranges from 0 to the value of `?GL_MAX_LIGHTS` \- 1.
`?GL_MAX_LIGHTS` is an implementation dependent constant that is greater than or
equal to eight. `Pname` specifies one of ten light source parameters, again by
symbolic name.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetLight.xml)

# `getMapdv`

```erlang
-spec getMapdv(Target :: enum(), Query :: enum(), V :: mem()) -> ok.
```

# `getMapfv`

```erlang
-spec getMapfv(Target :: enum(), Query :: enum(), V :: mem()) -> ok.
```

# `getMapiv`

```erlang
-spec getMapiv(Target :: enum(), Query :: enum(), V :: mem()) -> ok.
```

`glMap1` and `glMap2` define evaluators. [`gl:getMap()`](`getMapdv/3`) returns
evaluator parameters. `Target` chooses a map, `Query` selects a specific
parameter, and `V` points to storage where the values will be returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMap.xml)

# `getMaterialfv`

```erlang
-spec getMaterialfv(Face :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getMaterialiv`

```erlang
-spec getMaterialiv(Face :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getMaterial()`](`getMaterialfv/2`) returns in `Params` the value or values
of parameter `Pname` of material `Face`. Six parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMaterial.xml)

# `getMinmax`

```erlang
-spec getMinmax(Target :: enum(), Reset :: 0 | 1, Format :: enum(), Types :: enum(), Values :: mem()) ->
                   ok.
```

[`gl:getMinmax/5`](`getMinmax/5`) returns the accumulated minimum and maximum
pixel values (computed on a per-component basis) in a one-dimensional image of
width 2. The first set of return values are the minima, and the second set of
return values are the maxima. The format of the return values is determined by
`Format`, and their type is determined by `Types`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmax.xml)

# `getMinmaxParameterfv`

```erlang
-spec getMinmaxParameterfv(Target :: enum(), Pname :: enum()) -> {f()}.
```

# `getMinmaxParameteriv`

```erlang
-spec getMinmaxParameteriv(Target :: enum(), Pname :: enum()) -> {i()}.
```

[`gl:getMinmaxParameter()`](`getMinmaxParameterfv/2`) retrieves parameters for
the current minmax table by setting `Pname` to one of the following values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetMinmaxParameter.xml)

# `getMultisamplefv`

```erlang
-spec getMultisamplefv(Pname :: enum(), Index :: i()) -> {f(), f()}.
```

[`gl:getMultisamplefv/2`](`getMultisamplefv/2`) queries the location of a given
sample. `Pname` specifies the sample parameter to retrieve and must be
`?GL_SAMPLE_POSITION`. `Index` corresponds to the sample for which the location
should be returned. The sample location is returned as two floating-point values
in `Val[0]` and `Val[1]`, each between 0 and 1, corresponding to the `X` and `Y`
locations respectively in the GL pixel space of that sample. (0.5, 0.5) this
corresponds to the pixel center. `Index` must be between zero and the value of
`?GL_SAMPLES` minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetMultisample.xhtml)

# `getPixelMapfv`

```erlang
-spec getPixelMapfv(Map :: enum(), Values :: mem()) -> ok.
```

# `getPixelMapuiv`

```erlang
-spec getPixelMapuiv(Map :: enum(), Values :: mem()) -> ok.
```

# `getPixelMapusv`

```erlang
-spec getPixelMapusv(Map :: enum(), Values :: mem()) -> ok.
```

See the [`gl:pixelMap()`](`pixelMapfv/3`) reference page for a description of
the acceptable values for the `Map` parameter.
[`gl:getPixelMap()`](`getPixelMapfv/2`) returns in `Data` the contents of the
pixel map specified in `Map`. Pixel maps are used during the execution of
[`gl:readPixels/7`](`readPixels/7`), [`gl:drawPixels/5`](`drawPixels/5`),
[`gl:copyPixels/5`](`copyPixels/5`), [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`),
[`gl:texSubImage3D/11`](`texSubImage3D/11`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`), and
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`). to map color indices, stencil
indices, color components, and depth components to other values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPixelMap.xml)

# `getPolygonStipple`

```erlang
-spec getPolygonStipple() -> binary().
```

[`gl:getPolygonStipple/0`](`getPolygonStipple/0`) returns to `Pattern` a 32×32
polygon stipple pattern. The pattern is packed into memory as if
[`gl:readPixels/7`](`readPixels/7`) with both `height` and `width` of 32, `type`
of `?GL_BITMAP`, and `format` of `?GL_COLOR_INDEX` were called, and the stipple
pattern were stored in an internal 32×32 color index buffer. Unlike
[`gl:readPixels/7`](`readPixels/7`), however, pixel transfer operations (shift,
offset, pixel map) are not applied to the returned stipple image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetPolygonStipple.xml)

# `getProgramBinary`

```erlang
-spec getProgramBinary(Program :: i(), BufSize :: i()) -> {BinaryFormat :: enum(), Binary :: binary()}.
```

[`gl:getProgramBinary/2`](`getProgramBinary/2`) returns a binary representation
of the compiled and linked executable for `Program` into the array of bytes
whose address is specified in `Binary`. The maximum number of bytes that may be
written into `Binary` is specified by `BufSize`. If the program binary is
greater in size than `BufSize` bytes, then an error is generated, otherwise the
actual number of bytes written into `Binary` is returned in the variable whose
address is given by `Length`. If `Length` is `?NULL`, then no length is
returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramBinary.xhtml)

# `getProgramInfoLog`

```erlang
-spec getProgramInfoLog(Program :: i(), BufSize :: i()) -> string().
```

[`gl:getProgramInfoLog/2`](`getProgramInfoLog/2`) returns the information log
for the specified program object. The information log for a program object is
modified when the program object is linked or validated. The string that is
returned will be null terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramInfoLog.xhtml)

# `getProgramInterfaceiv`

```erlang
-spec getProgramInterfaceiv(Program :: i(), ProgramInterface :: enum(), Pname :: enum()) -> i().
```

[`gl:getProgramInterfaceiv/3`](`getProgramInterfaceiv/3`) queries the property
of the interface identifed by `ProgramInterface` in `Program`, the property name
of which is given by `Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramInterface.xhtml)

# `getProgramiv`

```erlang
-spec getProgramiv(Program :: i(), Pname :: enum()) -> i().
```

[`gl:getProgram()`](`getProgramiv/2`) returns in `Params` the value of a
parameter for a specific program object. The following parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml)

# `getProgramPipelineInfoLog`

```erlang
-spec getProgramPipelineInfoLog(Pipeline :: i(), BufSize :: i()) -> string().
```

[`gl:getProgramPipelineInfoLog/2`](`getProgramPipelineInfoLog/2`) retrieves the
info log for the program pipeline object `Pipeline`. The info log, including its
null terminator, is written into the array of characters whose address is given
by `InfoLog`. The maximum number of characters that may be written into
`InfoLog` is given by `BufSize`, and the actual number of characters written
into `InfoLog` is returned in the integer whose address is given by `Length`. If
`Length` is `?NULL`, no length is returned.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipelineInfoLog.xhtml)

# `getProgramPipelineiv`

```erlang
-spec getProgramPipelineiv(Pipeline :: i(), Pname :: enum()) -> i().
```

[`gl:getProgramPipelineiv/2`](`getProgramPipelineiv/2`) retrieves the value of a
property of the program pipeline object `Pipeline`. `Pname` specifies the name
of the parameter whose value to retrieve. The value of the parameter is written
to the variable whose address is given by `Params`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramPipeline.xhtml)

# `getProgramResourceIndex`

```erlang
-spec getProgramResourceIndex(Program :: i(), ProgramInterface :: enum(), Name :: string()) -> i().
```

[`gl:getProgramResourceIndex/3`](`getProgramResourceIndex/3`) returns the
unsigned integer index assigned to a resource named `Name` in the interface type
`ProgramInterface` of program object `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceIndex.xhtml)

# `getProgramResourceLocation`

```erlang
-spec getProgramResourceLocation(Program :: i(), ProgramInterface :: enum(), Name :: string()) -> i().
```

[`gl:getProgramResourceLocation/3`](`getProgramResourceLocation/3`) returns the
location assigned to the variable named `Name` in interface `ProgramInterface`
of program object `Program`. `Program` must be the name of a program that has
been linked successfully. `ProgramInterface` must be one of `?GL_UNIFORM`,
`?GL_PROGRAM_INPUT`, `?GL_PROGRAM_OUTPUT`, `?GL_VERTEX_SUBROUTINE_UNIFORM`,
`?GL_TESS_CONTROL_SUBROUTINE_UNIFORM`, `?GL_TESS_EVALUATION_SUBROUTINE_UNIFORM`,
`?GL_GEOMETRY_SUBROUTINE_UNIFORM`, `?GL_FRAGMENT_SUBROUTINE_UNIFORM`,
`?GL_COMPUTE_SUBROUTINE_UNIFORM`, or `?GL_TRANSFORM_FEEDBACK_BUFFER`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceLocation.xhtml)

# `getProgramResourceLocationIndex`

```erlang
-spec getProgramResourceLocationIndex(Program :: i(), ProgramInterface :: enum(), Name :: string()) ->
                                         i().
```

[`gl:getProgramResourceLocationIndex/3`](`getProgramResourceLocationIndex/3`)
returns the fragment color index assigned to the variable named `Name` in
interface `ProgramInterface` of program object `Program`. `Program` must be the
name of a program that has been linked successfully. `ProgramInterface` must be
`?GL_PROGRAM_OUTPUT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceLocationIndex.xhtml)

# `getProgramResourceName`

```erlang
-spec getProgramResourceName(Program :: i(), ProgramInterface :: enum(), Index :: i(), BufSize :: i()) ->
                                string().
```

[`gl:getProgramResourceName/4`](`getProgramResourceName/4`) retrieves the name
string assigned to the single active resource with an index of `Index` in the
interface `ProgramInterface` of program object `Program`. `Index` must be less
than the number of entries in the active resource list for `ProgramInterface`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramResourceName.xhtml)

# `getProgramStageiv`

```erlang
-spec getProgramStageiv(Program :: i(), Shadertype :: enum(), Pname :: enum()) -> i().
```

[`gl:getProgramStage()`](`getProgramStageiv/3`) queries a parameter of a shader
stage attached to a program object. `Program` contains the name of the program
to which the shader is attached. `Shadertype` specifies the stage from which to
query the parameter. `Pname` specifies which parameter should be queried. The
value or values of the parameter to be queried is returned in the variable whose
address is given in `Values`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgramStage.xhtml)

# `getQueryBufferObjecti64v`

```erlang
-spec getQueryBufferObjecti64v(Id :: i(), Buffer :: i(), Pname :: enum(), Offset :: i()) -> ok.
```

# `getQueryBufferObjectiv`

```erlang
-spec getQueryBufferObjectiv(Id :: i(), Buffer :: i(), Pname :: enum(), Offset :: i()) -> ok.
```

# `getQueryBufferObjectui64v`

```erlang
-spec getQueryBufferObjectui64v(Id :: i(), Buffer :: i(), Pname :: enum(), Offset :: i()) -> ok.
```

# `getQueryBufferObjectuiv`

```erlang
-spec getQueryBufferObjectuiv(Id :: i(), Buffer :: i(), Pname :: enum(), Offset :: i()) -> ok.
```

# `getQueryIndexediv`

```erlang
-spec getQueryIndexediv(Target :: enum(), Index :: i(), Pname :: enum()) -> i().
```

[`gl:getQueryIndexediv/3`](`getQueryIndexediv/3`) returns in `Params` a selected
parameter of the indexed query object target specified by `Target` and `Index`.
`Index` specifies the index of the query object target and must be between zero
and a target-specific maxiumum.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryIndexed.xhtml)

# `getQueryiv`

```erlang
-spec getQueryiv(Target :: enum(), Pname :: enum()) -> i().
```

[`gl:getQueryiv/2`](`getQueryiv/2`) returns in `Params` a selected parameter of
the query object target specified by `Target`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryiv.xhtml)

# `getQueryObjecti64v`

```erlang
-spec getQueryObjecti64v(Id :: i(), Pname :: enum()) -> i().
```

# `getQueryObjectiv`

```erlang
-spec getQueryObjectiv(Id :: i(), Pname :: enum()) -> i().
```

# `getQueryObjectui64v`

```erlang
-spec getQueryObjectui64v(Id :: i(), Pname :: enum()) -> i().
```

# `getQueryObjectuiv`

```erlang
-spec getQueryObjectuiv(Id :: i(), Pname :: enum()) -> i().
```

These commands return a selected parameter of the query object specified by
`Id`. [`gl:getQueryObject()`](`getQueryObjectiv/2`) returns in `Params` a
selected parameter of the query object specified by `Id`.
[`gl:getQueryBufferObject()`](`getQueryObjectiv/2`) returns in `Buffer` a
selected parameter of the query object specified by `Id`, by writing it to
`Buffer`'s data store at the byte offset specified by `Offset`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetQueryObject.xhtml)

# `getRenderbufferParameteriv`

```erlang
-spec getRenderbufferParameteriv(Target :: enum(), Pname :: enum()) -> i().
```

[`gl:getRenderbufferParameteriv/2`](`getRenderbufferParameteriv/2`) and
`glGetNamedRenderbufferParameteriv` query parameters of a specified renderbuffer
object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetRenderbufferParameter.xhtml)

# `getSamplerParameterfv`

```erlang
-spec getSamplerParameterfv(Sampler :: i(), Pname :: enum()) -> [f()].
```

# `getSamplerParameterIiv`

```erlang
-spec getSamplerParameterIiv(Sampler :: i(), Pname :: enum()) -> [i()].
```

# `getSamplerParameterIuiv`

```erlang
-spec getSamplerParameterIuiv(Sampler :: i(), Pname :: enum()) -> [i()].
```

# `getSamplerParameteriv`

```erlang
-spec getSamplerParameteriv(Sampler :: i(), Pname :: enum()) -> [i()].
```

[`gl:getSamplerParameter()`](`getSamplerParameteriv/2`) returns in `Params` the
value or values of the sampler parameter specified as `Pname`. `Sampler` defines
the target sampler, and must be the name of an existing sampler object, returned
from a previous call to [`gl:genSamplers/1`](`genSamplers/1`). `Pname` accepts
the same symbols as [`gl:samplerParameter()`](`samplerParameteri/3`), with the
same interpretations:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSamplerParameter.xhtml)

# `getShaderInfoLog`

```erlang
-spec getShaderInfoLog(Shader :: i(), BufSize :: i()) -> string().
```

[`gl:getShaderInfoLog/2`](`getShaderInfoLog/2`) returns the information log for
the specified shader object. The information log for a shader object is modified
when the shader is compiled. The string that is returned will be null
terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderInfoLog.xhtml)

# `getShaderiv`

```erlang
-spec getShaderiv(Shader :: i(), Pname :: enum()) -> i().
```

[`gl:getShader()`](`getShaderiv/2`) returns in `Params` the value of a parameter
for a specific shader object. The following parameters are defined:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShader.xhtml)

# `getShaderPrecisionFormat`

```erlang
-spec getShaderPrecisionFormat(Shadertype :: enum(), Precisiontype :: enum()) ->
                                  {Range :: {i(), i()}, Precision :: i()}.
```

[`gl:getShaderPrecisionFormat/2`](`getShaderPrecisionFormat/2`) retrieves the
numeric range and precision for the implementation's representation of
quantities in different numeric formats in specified shader type. `ShaderType`
specifies the type of shader for which the numeric precision and range is to be
retrieved and must be one of `?GL_VERTEX_SHADER` or `?GL_FRAGMENT_SHADER`.
`PrecisionType` specifies the numeric format to query and must be one of
`?GL_LOW_FLOAT`, `?GL_MEDIUM_FLOAT``?GL_HIGH_FLOAT`, `?GL_LOW_INT`,
`?GL_MEDIUM_INT`, or `?GL_HIGH_INT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderPrecisionFormat.xhtml)

# `getShaderSource`

```erlang
-spec getShaderSource(Shader :: i(), BufSize :: i()) -> string().
```

[`gl:getShaderSource/2`](`getShaderSource/2`) returns the concatenation of the
source code strings from the shader object specified by `Shader`. The source
code strings for a shader object are the result of a previous call to
[`gl:shaderSource/2`](`shaderSource/2`). The string returned by the function
will be null terminated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetShaderSource.xhtml)

# `getString`

```erlang
-spec getString(Name :: enum()) -> string().
```

# `getStringi`

```erlang
-spec getStringi(Name :: enum(), Index :: i()) -> string().
```

[`gl:getString/1`](`getString/1`) returns a pointer to a static string
describing some aspect of the current GL connection. `Name` can be one of the
following:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetString.xhtml)

# `getSubroutineIndex`

```erlang
-spec getSubroutineIndex(Program :: i(), Shadertype :: enum(), Name :: string()) -> i().
```

[`gl:getSubroutineIndex/3`](`getSubroutineIndex/3`) returns the index of a
subroutine uniform within a shader stage attached to a program object. `Program`
contains the name of the program to which the shader is attached. `Shadertype`
specifies the stage from which to query shader subroutine index. `Name` contains
the null-terminated name of the subroutine uniform whose name to query.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineIndex.xhtml)

# `getSubroutineUniformLocation`

```erlang
-spec getSubroutineUniformLocation(Program :: i(), Shadertype :: enum(), Name :: string()) -> i().
```

[`gl:getSubroutineUniformLocation/3`](`getSubroutineUniformLocation/3`) returns
the location of the subroutine uniform variable `Name` in the shader stage of
type `Shadertype` attached to `Program`, with behavior otherwise identical to
[`gl:getUniformLocation/2`](`getUniformLocation/2`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSubroutineUniformLocation.xhtml)

# `getSynciv`

```erlang
-spec getSynciv(Sync :: i(), Pname :: enum(), BufSize :: i()) -> [i()].
```

[`gl:getSynciv/3`](`getSynciv/3`) retrieves properties of a sync object. `Sync`
specifies the name of the sync object whose properties to retrieve.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetSync.xhtml)

# `getTexEnvfv`

```erlang
-spec getTexEnvfv(Target :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getTexEnviv`

```erlang
-spec getTexEnviv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getTexEnv()`](`getTexEnvfv/2`) returns in `Params` selected values of a
texture environment that was specified with [`gl:texEnv()`](`texEnvf/3`).
`Target` specifies a texture environment.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexEnv.xml)

# `getTexGendv`

```erlang
-spec getTexGendv(Coord :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getTexGenfv`

```erlang
-spec getTexGenfv(Coord :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getTexGeniv`

```erlang
-spec getTexGeniv(Coord :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getTexGen()`](`getTexGendv/2`) returns in `Params` selected parameters of a
texture coordinate generation function that was specified using
[`gl:texGen()`](`texGend/3`). `Coord` names one of the (`s`, `t`, `r`, `q`)
texture coordinates, using the symbolic constant `?GL_S`, `?GL_T`, `?GL_R`, or
`?GL_Q`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGetTexGen.xml)

# `getTexImage`

```erlang
-spec getTexImage(Target :: enum(), Level :: i(), Format :: enum(), Type :: enum(), Pixels :: mem()) ->
                     ok.
```

[`gl:getTexImage/5`](`getTexImage/5`), `glGetnTexImage` and `glGetTextureImage`
functions return a texture image into `Pixels`. For
[`gl:getTexImage/5`](`getTexImage/5`) and `glGetnTexImage`, `Target` specifies
whether the desired texture image is one specified by
[`gl:texImage1D/8`](`texImage1D/8`) (`?GL_TEXTURE_1D`),
[`gl:texImage2D/9`](`texImage2D/9`) (`?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_2D` or any of `?GL_TEXTURE_CUBE_MAP_*`),
or [`gl:texImage3D/10`](`texImage3D/10`) (`?GL_TEXTURE_2D_ARRAY`,
`?GL_TEXTURE_3D`, `?GL_TEXTURE_CUBE_MAP_ARRAY`). For `glGetTextureImage`,
`Texture` specifies the texture object name. In addition to types of textures
accepted by [`gl:getTexImage/5`](`getTexImage/5`) and `glGetnTexImage`, the
function also accepts cube map texture objects (with effective target
`?GL_TEXTURE_CUBE_MAP`). `Level` specifies the level-of-detail number of the
desired image. `Format` and `Type` specify the format and type of the desired
image array. See the reference page for [`gl:texImage1D/8`](`texImage1D/8`) for
a description of the acceptable values for the `Format` and `Type` parameters,
respectively. For glGetnTexImage and glGetTextureImage functions, bufSize tells
the size of the buffer to receive the retrieved pixel data. `glGetnTexImage` and
`glGetTextureImage` do not write more than `BufSize` bytes into `Pixels`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexImage.xhtml)

# `getTexLevelParameterfv`

```erlang
-spec getTexLevelParameterfv(Target :: enum(), Level :: i(), Pname :: enum()) -> {f()}.
```

# `getTexLevelParameteriv`

```erlang
-spec getTexLevelParameteriv(Target :: enum(), Level :: i(), Pname :: enum()) -> {i()}.
```

[`gl:getTexLevelParameterfv/3`](`getTexLevelParameterfv/3`),
[`gl:getTexLevelParameteriv/3`](`getTexLevelParameterfv/3`),
`glGetTextureLevelParameterfv` and `glGetTextureLevelParameteriv` return in
`Params` texture parameter values for a specific level-of-detail value,
specified as `Level`. For the first two functions, `Target` defines the target
texture, either `?GL_TEXTURE_1D`, `?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`,
`?GL_PROXY_TEXTURE_1D`, `?GL_PROXY_TEXTURE_2D`, `?GL_PROXY_TEXTURE_3D`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_X`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_X`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_Y`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Y`,
`?GL_TEXTURE_CUBE_MAP_POSITIVE_Z`, `?GL_TEXTURE_CUBE_MAP_NEGATIVE_Z`, or
`?GL_PROXY_TEXTURE_CUBE_MAP`. The remaining two take a `Texture` argument which
specifies the name of the texture object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexLevelParameter.xhtml)

# `getTexParameterfv`

```erlang
-spec getTexParameterfv(Target :: enum(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getTexParameterIiv`

```erlang
-spec getTexParameterIiv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

# `getTexParameterIuiv`

```erlang
-spec getTexParameterIuiv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

# `getTexParameteriv`

```erlang
-spec getTexParameteriv(Target :: enum(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getTexParameter()`](`getTexParameterfv/2`) and `glGetTextureParameter`
return in `Params` the value or values of the texture parameter specified as
`Pname`. `Target` defines the target texture. `?GL_TEXTURE_1D`,
`?GL_TEXTURE_2D`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_1D_ARRAY`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_RECTANGLE`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, `?GL_TEXTURE_2D_MULTISAMPLE`, or
`?GL_TEXTURE_2D_MULTISAMPLE_ARRAY` specify one-, two-, or three-dimensional,
one-dimensional array, two-dimensional array, rectangle, cube-mapped or
cube-mapped array, two-dimensional multisample, or two-dimensional multisample
array texturing, respectively. `Pname` accepts the same symbols as
[`gl:texParameter()`](`texParameterf/3`), with the same interpretations:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTexParameter.xhtml)

# `getTransformFeedbackVarying`

```erlang
-spec getTransformFeedbackVarying(Program :: i(), Index :: i(), BufSize :: i()) ->
                                     {Size :: i(), Type :: enum(), Name :: string()}.
```

Information about the set of varying variables in a linked program that will be
captured during transform feedback may be retrieved by calling
[`gl:getTransformFeedbackVarying/3`](`getTransformFeedbackVarying/3`).
[`gl:getTransformFeedbackVarying/3`](`getTransformFeedbackVarying/3`) provides
information about the varying variable selected by `Index`. An `Index` of 0
selects the first varying variable specified in the `Varyings` array passed to
[`gl:transformFeedbackVaryings/3`](`transformFeedbackVaryings/3`), and an
`Index` of the value of `?GL_TRANSFORM_FEEDBACK_VARYINGS` minus one selects the
last such variable.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetTransformFeedbackVarying.xhtml)

# `getUniformBlockIndex`

```erlang
-spec getUniformBlockIndex(Program :: i(), UniformBlockName :: string()) -> i().
```

[`gl:getUniformBlockIndex/2`](`getUniformBlockIndex/2`) retrieves the index of a
uniform block within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformBlockIndex.xhtml)

# `getUniformdv`

```erlang
-spec getUniformdv(Program :: i(), Location :: i()) -> matrix().
```

# `getUniformfv`

```erlang
-spec getUniformfv(Program :: i(), Location :: i()) -> matrix().
```

# `getUniformIndices`

```erlang
-spec getUniformIndices(Program :: i(), UniformNames :: [unicode:chardata()]) -> [i()].
```

[`gl:getUniformIndices/2`](`getUniformIndices/2`) retrieves the indices of a
number of uniforms within `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformIndices.xhtml)

# `getUniformiv`

```erlang
-spec getUniformiv(Program :: i(), Location :: i()) ->
                      {i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i()}.
```

# `getUniformLocation`

```erlang
-spec getUniformLocation(Program :: i(), Name :: string()) -> i().
```

`glGetUniformLocation `returns an integer that represents the location of a
specific uniform variable within a program object. `Name` must be a null
terminated string that contains no white space. `Name` must be an active uniform
variable name in `Program` that is not a structure, an array of structures, or a
subcomponent of a vector or a matrix. This function returns -1 if `Name` does
not correspond to an active uniform variable in `Program`, if `Name` starts with
the reserved prefix "gl\_", or if `Name` is associated with an atomic counter or
a named uniform block.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformLocation.xhtml)

# `getUniformSubroutineuiv`

```erlang
-spec getUniformSubroutineuiv(Shadertype :: enum(), Location :: i()) ->
                                 {i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i(),
                                  i()}.
```

[`gl:getUniformSubroutine()`](`getUniformSubroutineuiv/2`) retrieves the value
of the subroutine uniform at location `Location` for shader stage `Shadertype`
of the current program. `Location` must be less than the value of
`?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS` for the shader currently in use at
shader stage `Shadertype`. The value of the subroutine uniform is returned in
`Values`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformSubroutine.xhtml)

# `getUniformuiv`

```erlang
-spec getUniformuiv(Program :: i(), Location :: i()) ->
                       {i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i(), i()}.
```

[`gl:getUniform()`](`getUniformfv/2`) and `glGetnUniform` return in `Params` the
value(s) of the specified uniform variable. The type of the uniform variable
specified by `Location` determines the number of values returned. If the uniform
variable is defined in the shader as a boolean, int, or float, a single value
will be returned. If it is defined as a vec2, ivec2, or bvec2, two values will
be returned. If it is defined as a vec3, ivec3, or bvec3, three values will be
returned, and so on. To query values stored in uniform variables declared as
arrays, call [`gl:getUniform()`](`getUniformfv/2`) for each element of the
array. To query values stored in uniform variables declared as structures, call
[`gl:getUniform()`](`getUniformfv/2`) for each field in the structure. The
values for uniform variables declared as a matrix will be returned in column
major order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniform.xhtml)

# `getVertexAttribdv`

```erlang
-spec getVertexAttribdv(Index :: i(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getVertexAttribfv`

```erlang
-spec getVertexAttribfv(Index :: i(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `getVertexAttribIiv`

```erlang
-spec getVertexAttribIiv(Index :: i(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

# `getVertexAttribIuiv`

```erlang
-spec getVertexAttribIuiv(Index :: i(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

# `getVertexAttribiv`

```erlang
-spec getVertexAttribiv(Index :: i(), Pname :: enum()) -> {i(), i(), i(), i()}.
```

[`gl:getVertexAttrib()`](`getVertexAttribdv/2`) returns in `Params` the value of
a generic vertex attribute parameter. The generic vertex attribute to be queried
is specified by `Index`, and the parameter to be queried is specified by
`Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetVertexAttrib.xhtml)

# `getVertexAttribLdv`

```erlang
-spec getVertexAttribLdv(Index :: i(), Pname :: enum()) -> {f(), f(), f(), f()}.
```

# `hint`

```erlang
-spec hint(Target :: enum(), Mode :: enum()) -> ok.
```

Certain aspects of GL behavior, when there is room for interpretation, can be
controlled with hints. A hint is specified with two arguments. `Target` is a
symbolic constant indicating the behavior to be controlled, and `Mode` is
another symbolic constant indicating the desired behavior. The initial value for
each `Target` is `?GL_DONT_CARE`. `Mode` can be one of the following:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glHint.xhtml)

# `histogram`

```erlang
-spec histogram(Target :: enum(), Width :: i(), Internalformat :: enum(), Sink :: 0 | 1) -> ok.
```

When `?GL_HISTOGRAM` is enabled, RGBA color components are converted to
histogram table indices by clamping to the range \[0,1], multiplying by the
width of the histogram table, and rounding to the nearest integer. The table
entries selected by the RGBA indices are then incremented. (If the internal
format of the histogram table includes luminance, then the index derived from
the R color component determines the luminance table entry to be incremented.)
If a histogram table entry is incremented beyond its maximum value, then its
value becomes undefined. (This is not an error.)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glHistogram.xml)

# `indexd`

```erlang
-spec indexd(C :: f()) -> ok.
```

# `indexdv`

```erlang
-spec indexdv({C :: f()}) -> ok.
```

# `indexf`

```erlang
-spec indexf(C :: f()) -> ok.
```

# `indexfv`

```erlang
-spec indexfv({C :: f()}) -> ok.
```

# `indexi`

```erlang
-spec indexi(C :: i()) -> ok.
```

# `indexiv`

```erlang
-spec indexiv({C :: i()}) -> ok.
```

# `indexMask`

```erlang
-spec indexMask(Mask :: i()) -> ok.
```

[`gl:indexMask/1`](`indexMask/1`) controls the writing of individual bits in the
color index buffers. The least significant n bits of `Mask`, where n is the
number of bits in a color index buffer, specify a mask. Where a 1 (one) appears
in the mask, it's possible to write to the corresponding bit in the color index
buffer (or buffers). Where a 0 (zero) appears, the corresponding bit is
write-protected.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexMask.xml)

# `indexPointer`

```erlang
-spec indexPointer(Type :: enum(), Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:indexPointer/3`](`indexPointer/3`) specifies the location and data format
of an array of color indexes to use when rendering. `Type` specifies the data
type of each color index and `Stride` specifies the byte stride from one color
index to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndexPointer.xml)

# `indexs`

```erlang
-spec indexs(C :: i()) -> ok.
```

# `indexsv`

```erlang
-spec indexsv({C :: i()}) -> ok.
```

# `indexub`

```erlang
-spec indexub(C :: i()) -> ok.
```

# `indexubv`

```erlang
-spec indexubv({C :: i()}) -> ok.
```

[`gl:index()`](`indexd/1`) updates the current (single-valued) color index. It
takes one argument, the new value for the current color index.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIndex.xml)

# `initNames`

```erlang
-spec initNames() -> ok.
```

The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers.
[`gl:initNames/0`](`initNames/0`) causes the name stack to be initialized to its
default empty state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInitNames.xml)

# `interleavedArrays`

```erlang
-spec interleavedArrays(Format :: enum(), Stride :: i(), Pointer :: offset() | mem()) -> ok.
```

[`gl:interleavedArrays/3`](`interleavedArrays/3`) lets you specify and enable
individual color, normal, texture and vertex arrays whose elements are part of a
larger aggregate array element. For some implementations, this is more efficient
than specifying the arrays separately.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glInterleavedArrays.xml)

# `invalidateBufferData`

```erlang
-spec invalidateBufferData(Buffer :: i()) -> ok.
```

[`gl:invalidateBufferData/1`](`invalidateBufferData/1`) invalidates all of the
content of the data store of a buffer object. After invalidation, the content of
the buffer's data store becomes undefined.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateBufferData.xhtml)

# `invalidateBufferSubData`

```erlang
-spec invalidateBufferSubData(Buffer :: i(), Offset :: i(), Length :: i()) -> ok.
```

[`gl:invalidateBufferSubData/3`](`invalidateBufferSubData/3`) invalidates all or
part of the content of the data store of a buffer object. After invalidation,
the content of the specified range of the buffer's data store becomes undefined.
The start of the range is given by `Offset` and its size is given by `Length`,
both measured in basic machine units.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateBufferSubData.xhtml)

# `invalidateFramebuffer`

```erlang
-spec invalidateFramebuffer(Target :: enum(), Attachments :: [enum()]) -> ok.
```

[`gl:invalidateFramebuffer/2`](`invalidateFramebuffer/2`) and
`glInvalidateNamedFramebufferData` invalidate the entire contents of a specified
set of attachments of a framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateFramebuffer.xhtml)

# `invalidateSubFramebuffer`

```erlang
-spec invalidateSubFramebuffer(Target :: enum(),
                               Attachments :: [enum()],
                               X :: i(),
                               Y :: i(),
                               Width :: i(),
                               Height :: i()) ->
                                  ok.
```

[`gl:invalidateSubFramebuffer/6`](`invalidateSubFramebuffer/6`) and
`glInvalidateNamedFramebufferSubData` invalidate the contents of a specified
region of a specified set of attachments of a framebuffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateSubFramebuffer.xhtml)

# `invalidateTexImage`

```erlang
-spec invalidateTexImage(Texture :: i(), Level :: i()) -> ok.
```

[`gl:invalidateTexSubImage/8`](`invalidateTexSubImage/8`) invalidates all of a
texture image. `Texture` and `Level` indicated which texture image is being
invalidated. After this command, data in the texture image has undefined values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateTexImage.xhtml)

# `invalidateTexSubImage`

```erlang
-spec invalidateTexSubImage(Texture, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth) -> ok
                               when
                                   Texture :: i(),
                                   Level :: i(),
                                   Xoffset :: i(),
                                   Yoffset :: i(),
                                   Zoffset :: i(),
                                   Width :: i(),
                                   Height :: i(),
                                   Depth :: i().
```

[`gl:invalidateTexSubImage/8`](`invalidateTexSubImage/8`) invalidates all or
part of a texture image. `Texture` and `Level` indicated which texture image is
being invalidated. After this command, data in that subregion have undefined
values. `Xoffset`, `Yoffset`, `Zoffset`, `Width`, `Height`, and `Depth` are
interpreted as they are in [`gl:texSubImage3D/11`](`texSubImage3D/11`). For
texture targets that don't have certain dimensions, this command treats those
dimensions as having a size of 1. For example, to invalidate a portion of a two-
dimensional texture, the application would use `Zoffset` equal to zero and
`Depth` equal to one. Cube map textures are treated as an array of six slices in
the z-dimension, where a value of `Zoffset` is interpreted as specifying face
`?GL_TEXTURE_CUBE_MAP_POSITIVE_X` \+ `Zoffset`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glInvalidateTexSubImage.xhtml)

# `isBuffer`

```erlang
-spec isBuffer(Buffer :: i()) -> 0 | 1.
```

[`gl:isBuffer/1`](`isBuffer/1`) returns `?GL_TRUE` if `Buffer` is currently the
name of a buffer object. If `Buffer` is zero, or is a non-zero value that is not
currently the name of a buffer object, or if an error occurs,
[`gl:isBuffer/1`](`isBuffer/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsBuffer.xhtml)

# `isEnabled`

```erlang
-spec isEnabled(Cap :: enum()) -> 0 | 1.
```

# `isEnabledi`

```erlang
-spec isEnabledi(Target :: enum(), Index :: i()) -> 0 | 1.
```

[`gl:isEnabled/1`](`isEnabled/1`) returns `?GL_TRUE` if `Cap` is an enabled
capability and returns `?GL_FALSE` otherwise. Boolean states that are indexed
may be tested with [`gl:isEnabledi/2`](`isEnabled/1`). For
[`gl:isEnabledi/2`](`isEnabled/1`), `Index` specifies the index of the
capability to test. `Index` must be between zero and the count of indexed
capabilities for `Cap`. Initially all capabilities except `?GL_DITHER` are
disabled; `?GL_DITHER` is initially enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsEnabled.xhtml)

# `isFramebuffer`

```erlang
-spec isFramebuffer(Framebuffer :: i()) -> 0 | 1.
```

[`gl:isFramebuffer/1`](`isFramebuffer/1`) returns `?GL_TRUE` if `Framebuffer` is
currently the name of a framebuffer object. If `Framebuffer` is zero, or if
`?framebuffer` is not the name of a framebuffer object, or if an error occurs,
[`gl:isFramebuffer/1`](`isFramebuffer/1`) returns `?GL_FALSE`. If `Framebuffer`
is a name returned by [`gl:genFramebuffers/1`](`genFramebuffers/1`), by that has
not yet been bound through a call to
[`gl:bindFramebuffer/2`](`bindFramebuffer/2`), then the name is not a
framebuffer object and [`gl:isFramebuffer/1`](`isFramebuffer/1`) returns
`?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsFramebuffer.xhtml)

# `isList`

```erlang
-spec isList(List :: i()) -> 0 | 1.
```

[`gl:isList/1`](`isList/1`) returns `?GL_TRUE` if `List` is the name of a
display list and returns `?GL_FALSE` if it is not, or if an error occurs.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glIsList.xml)

# `isProgram`

```erlang
-spec isProgram(Program :: i()) -> 0 | 1.
```

[`gl:isProgram/1`](`isProgram/1`) returns `?GL_TRUE` if `Program` is the name of
a program object previously created with
[`gl:createProgram/0`](`createProgram/0`) and not yet deleted with
[`gl:deleteProgram/1`](`deleteProgram/1`). If `Program` is zero or a non-zero
value that is not the name of a program object, or if an error occurs,
[`gl:isProgram/1`](`isProgram/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgram.xhtml)

# `isProgramPipeline`

```erlang
-spec isProgramPipeline(Pipeline :: i()) -> 0 | 1.
```

[`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns `?GL_TRUE` if
`Pipeline` is currently the name of a program pipeline object. If `Pipeline` is
zero, or if `?pipeline` is not the name of a program pipeline object, or if an
error occurs, [`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns
`?GL_FALSE`. If `Pipeline` is a name returned by
[`gl:genProgramPipelines/1`](`genProgramPipelines/1`), but that has not yet been
bound through a call to [`gl:bindProgramPipeline/1`](`bindProgramPipeline/1`),
then the name is not a program pipeline object and
[`gl:isProgramPipeline/1`](`isProgramPipeline/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsProgramPipeline.xhtml)

# `isQuery`

```erlang
-spec isQuery(Id :: i()) -> 0 | 1.
```

[`gl:isQuery/1`](`isQuery/1`) returns `?GL_TRUE` if `Id` is currently the name
of a query object. If `Id` is zero, or is a non-zero value that is not currently
the name of a query object, or if an error occurs, [`gl:isQuery/1`](`isQuery/1`)
returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsQuery.xhtml)

# `isRenderbuffer`

```erlang
-spec isRenderbuffer(Renderbuffer :: i()) -> 0 | 1.
```

[`gl:isRenderbuffer/1`](`isRenderbuffer/1`) returns `?GL_TRUE` if `Renderbuffer`
is currently the name of a renderbuffer object. If `Renderbuffer` is zero, or if
`Renderbuffer` is not the name of a renderbuffer object, or if an error occurs,
[`gl:isRenderbuffer/1`](`isRenderbuffer/1`) returns `?GL_FALSE`. If
`Renderbuffer` is a name returned by
[`gl:genRenderbuffers/1`](`genRenderbuffers/1`), by that has not yet been bound
through a call to [`gl:bindRenderbuffer/2`](`bindRenderbuffer/2`) or
[`gl:framebufferRenderbuffer/4`](`framebufferRenderbuffer/4`), then the name is
not a renderbuffer object and [`gl:isRenderbuffer/1`](`isRenderbuffer/1`)
returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsRenderbuffer.xhtml)

# `isSampler`

```erlang
-spec isSampler(Sampler :: i()) -> 0 | 1.
```

[`gl:isSampler/1`](`isSampler/1`) returns `?GL_TRUE` if `Id` is currently the
name of a sampler object. If `Id` is zero, or is a non-zero value that is not
currently the name of a sampler object, or if an error occurs,
[`gl:isSampler/1`](`isSampler/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSampler.xhtml)

# `isShader`

```erlang
-spec isShader(Shader :: i()) -> 0 | 1.
```

[`gl:isShader/1`](`isShader/1`) returns `?GL_TRUE` if `Shader` is the name of a
shader object previously created with [`gl:createShader/1`](`createShader/1`)
and not yet deleted with [`gl:deleteShader/1`](`deleteShader/1`). If `Shader` is
zero or a non-zero value that is not the name of a shader object, or if an error
occurs, `glIsShader `returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsShader.xhtml)

# `isSync`

```erlang
-spec isSync(Sync :: i()) -> 0 | 1.
```

[`gl:isSync/1`](`isSync/1`) returns `?GL_TRUE` if `Sync` is currently the name
of a sync object. If `Sync` is not the name of a sync object, or if an error
occurs, [`gl:isSync/1`](`isSync/1`) returns `?GL_FALSE`. Note that zero is not
the name of a sync object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsSync.xhtml)

# `isTexture`

```erlang
-spec isTexture(Texture :: i()) -> 0 | 1.
```

[`gl:isTexture/1`](`isTexture/1`) returns `?GL_TRUE` if `Texture` is currently
the name of a texture. If `Texture` is zero, or is a non-zero value that is not
currently the name of a texture, or if an error occurs,
[`gl:isTexture/1`](`isTexture/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTexture.xhtml)

# `isTransformFeedback`

```erlang
-spec isTransformFeedback(Id :: i()) -> 0 | 1.
```

[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_TRUE` if `Id`
is currently the name of a transform feedback object. If `Id` is zero, or if
`?id` is not the name of a transform feedback object, or if an error occurs,
[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_FALSE`. If
`Id` is a name returned by
[`gl:genTransformFeedbacks/1`](`genTransformFeedbacks/1`), but that has not yet
been bound through a call to
[`gl:bindTransformFeedback/2`](`bindTransformFeedback/2`), then the name is not
a transform feedback object and
[`gl:isTransformFeedback/1`](`isTransformFeedback/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsTransformFeedback.xhtml)

# `isVertexArray`

```erlang
-spec isVertexArray(Array :: i()) -> 0 | 1.
```

[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_TRUE` if `Array` is
currently the name of a vertex array object. If `Array` is zero, or if `Array`
is not the name of a vertex array object, or if an error occurs,
[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_FALSE`. If `Array` is a
name returned by [`gl:genVertexArrays/1`](`genVertexArrays/1`), by that has not
yet been bound through a call to [`gl:bindVertexArray/1`](`bindVertexArray/1`),
then the name is not a vertex array object and
[`gl:isVertexArray/1`](`isVertexArray/1`) returns `?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glIsVertexArray.xhtml)

# `lightf`

```erlang
-spec lightf(Light :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `lightfv`

```erlang
-spec lightfv(Light :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `lighti`

```erlang
-spec lighti(Light :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

# `lightiv`

```erlang
-spec lightiv(Light :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:light()`](`lightf/3`) sets the values of individual light source
parameters. `Light` names the light and is a symbolic name of the form
`?GL_LIGHT` i, where i ranges from 0 to the value of `?GL_MAX_LIGHTS` \- 1.
`Pname` specifies one of ten light source parameters, again by symbolic name.
`Params` is either a single value or a pointer to an array that contains the new
values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLight.xml)

# `lightModelf`

```erlang
-spec lightModelf(Pname :: enum(), Param :: f()) -> ok.
```

# `lightModelfv`

```erlang
-spec lightModelfv(Pname :: enum(), Params :: tuple()) -> ok.
```

# `lightModeli`

```erlang
-spec lightModeli(Pname :: enum(), Param :: i()) -> ok.
```

# `lightModeliv`

```erlang
-spec lightModeliv(Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:lightModel()`](`lightModelf/2`) sets the lighting model parameter. `Pname`
names a parameter and `Params` gives the new value. There are three lighting
model parameters:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLightModel.xml)

# `lineStipple`

```erlang
-spec lineStipple(Factor :: i(), Pattern :: i()) -> ok.
```

Line stippling masks out certain fragments produced by rasterization; those
fragments will not be drawn. The masking is achieved by using three parameters:
the 16-bit line stipple pattern `Pattern`, the repeat count `Factor`, and an
integer stipple counter s.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLineStipple.xml)

# `lineWidth`

```erlang
-spec lineWidth(Width :: f()) -> ok.
```

[`gl:lineWidth/1`](`lineWidth/1`) specifies the rasterized width of both aliased
and antialiased lines. Using a line width other than 1 has different effects,
depending on whether line antialiasing is enabled. To enable and disable line
antialiasing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_LINE_SMOOTH`. Line antialiasing is initially disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLineWidth.xhtml)

# `linkProgram`

```erlang
-spec linkProgram(Program :: i()) -> ok.
```

[`gl:linkProgram/1`](`linkProgram/1`) links the program object specified by
`Program`. If any shader objects of type `?GL_VERTEX_SHADER` are attached to
`Program`, they will be used to create an executable that will run on the
programmable vertex processor. If any shader objects of type
`?GL_GEOMETRY_SHADER` are attached to `Program`, they will be used to create an
executable that will run on the programmable geometry processor. If any shader
objects of type `?GL_FRAGMENT_SHADER` are attached to `Program`, they will be
used to create an executable that will run on the programmable fragment
processor.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLinkProgram.xhtml)

# `listBase`

```erlang
-spec listBase(Base :: i()) -> ok.
```

[`gl:callLists/1`](`callLists/1`) specifies an array of offsets. Display-list
names are generated by adding `Base` to each offset. Names that reference valid
display lists are executed; the others are ignored.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glListBase.xml)

# `loadIdentity`

```erlang
-spec loadIdentity() -> ok.
```

[`gl:loadIdentity/0`](`loadIdentity/0`) replaces the current matrix with the
identity matrix. It is semantically equivalent to calling
[`gl:loadMatrix()`](`loadMatrixd/1`) with the identity matrix

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadIdentity.xml)

# `loadMatrixd`

```erlang
-spec loadMatrixd(M :: matrix()) -> ok.
```

# `loadMatrixf`

```erlang
-spec loadMatrixf(M :: matrix()) -> ok.
```

[`gl:loadMatrix()`](`loadMatrixd/1`) replaces the current matrix with the one
whose elements are specified by `M`. The current matrix is the projection
matrix, modelview matrix, or texture matrix, depending on the current matrix
mode (see [`gl:matrixMode/1`](`matrixMode/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadMatrix.xml)

# `loadName`

```erlang
-spec loadName(Name :: i()) -> ok.
```

The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers
and is initially empty.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadName.xml)

# `loadTransposeMatrixd`

```erlang
-spec loadTransposeMatrixd(M :: matrix()) -> ok.
```

# `loadTransposeMatrixf`

```erlang
-spec loadTransposeMatrixf(M :: matrix()) -> ok.
```

[`gl:loadTransposeMatrix()`](`loadTransposeMatrixf/1`) replaces the current
matrix with the one whose elements are specified by `M`. The current matrix is
the projection matrix, modelview matrix, or texture matrix, depending on the
current matrix mode (see [`gl:matrixMode/1`](`matrixMode/1`)).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glLoadTransposeMatrix.xml)

# `logicOp`

```erlang
-spec logicOp(Opcode :: enum()) -> ok.
```

[`gl:logicOp/1`](`logicOp/1`) specifies a logical operation that, when enabled,
is applied between the incoming RGBA color and the RGBA color at the
corresponding location in the frame buffer. To enable or disable the logical
operation, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
using the symbolic constant `?GL_COLOR_LOGIC_OP`. The initial value is disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glLogicOp.xhtml)

# `map1d`

```erlang
-spec map1d(Target :: enum(), U1 :: f(), U2 :: f(), Stride :: i(), Order :: i(), Points :: binary()) ->
               ok.
```

# `map1f`

```erlang
-spec map1f(Target :: enum(), U1 :: f(), U2 :: f(), Stride :: i(), Order :: i(), Points :: binary()) ->
               ok.
```

Evaluators provide a way to use polynomial or rational polynomial mapping to
produce vertices, normals, texture coordinates, and colors. The values produced
by an evaluator are sent to further stages of GL processing just as if they had
been presented using [`gl:vertex()`](`vertex2d/2`),
[`gl:normal()`](`normal3b/3`), [`gl:texCoord()`](`texCoord1d/1`), and
[`gl:color()`](`color3b/3`) commands, except that the generated values do not
update the current normal, texture coordinates, or color.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMap1.xml)

# `map2d`

```erlang
-spec map2d(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> ok
               when
                   Target :: enum(),
                   U1 :: f(),
                   U2 :: f(),
                   Ustride :: i(),
                   Uorder :: i(),
                   V1 :: f(),
                   V2 :: f(),
                   Vstride :: i(),
                   Vorder :: i(),
                   Points :: binary().
```

# `map2f`

```erlang
-spec map2f(Target, U1, U2, Ustride, Uorder, V1, V2, Vstride, Vorder, Points) -> ok
               when
                   Target :: enum(),
                   U1 :: f(),
                   U2 :: f(),
                   Ustride :: i(),
                   Uorder :: i(),
                   V1 :: f(),
                   V2 :: f(),
                   Vstride :: i(),
                   Vorder :: i(),
                   Points :: binary().
```

Evaluators provide a way to use polynomial or rational polynomial mapping to
produce vertices, normals, texture coordinates, and colors. The values produced
by an evaluator are sent on to further stages of GL processing just as if they
had been presented using [`gl:vertex()`](`vertex2d/2`),
[`gl:normal()`](`normal3b/3`), [`gl:texCoord()`](`texCoord1d/1`), and
[`gl:color()`](`color3b/3`) commands, except that the generated values do not
update the current normal, texture coordinates, or color.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMap2.xml)

# `mapGrid1d`

```erlang
-spec mapGrid1d(Un :: i(), U1 :: f(), U2 :: f()) -> ok.
```

# `mapGrid1f`

```erlang
-spec mapGrid1f(Un :: i(), U1 :: f(), U2 :: f()) -> ok.
```

# `mapGrid2d`

```erlang
-spec mapGrid2d(Un :: i(), U1 :: f(), U2 :: f(), Vn :: i(), V1 :: f(), V2 :: f()) -> ok.
```

# `mapGrid2f`

```erlang
-spec mapGrid2f(Un :: i(), U1 :: f(), U2 :: f(), Vn :: i(), V1 :: f(), V2 :: f()) -> ok.
```

[`gl:mapGrid()`](`mapGrid1d/3`) and [`gl:evalMesh()`](`evalMesh1/3`) are used
together to efficiently generate and evaluate a series of evenly-spaced map
domain values. [`gl:evalMesh()`](`evalMesh1/3`) steps through the integer domain
of a one- or two-dimensional grid, whose range is the domain of the evaluation
maps specified by `glMap1` and `glMap2`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMapGrid.xml)

# `materialf`

```erlang
-spec materialf(Face :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `materialfv`

```erlang
-spec materialfv(Face :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `materiali`

```erlang
-spec materiali(Face :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

# `materialiv`

```erlang
-spec materialiv(Face :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:material()`](`materialf/3`) assigns values to material parameters. There
are two matched sets of material parameters. One, the `front-facing` set, is
used to shade points, lines, bitmaps, and all polygons (when two-sided lighting
is disabled), or just front-facing polygons (when two-sided lighting is
enabled). The other set, `back-facing`, is used to shade back-facing polygons
only when two-sided lighting is enabled. Refer to the
[`gl:lightModel()`](`lightModelf/2`) reference page for details concerning one-
and two-sided lighting calculations.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMaterial.xml)

# `matrixMode`

```erlang
-spec matrixMode(Mode :: enum()) -> ok.
```

[`gl:matrixMode/1`](`matrixMode/1`) sets the current matrix mode. `Mode` can
assume one of four values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMatrixMode.xml)

# `memoryBarrier`

```erlang
-spec memoryBarrier(Barriers :: i()) -> ok.
```

# `memoryBarrierByRegion`

```erlang
-spec memoryBarrierByRegion(Barriers :: i()) -> ok.
```

[`gl:memoryBarrier/1`](`memoryBarrier/1`) defines a barrier ordering the memory
transactions issued prior to the command relative to those issued after the
barrier. For the purposes of this ordering, memory transactions performed by
shaders are considered to be issued by the rendering command that triggered the
execution of the shader. `Barriers` is a bitfield indicating the set of
operations that are synchronized with shader stores; the bits used in `Barriers`
are as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMemoryBarrier.xhtml)

# `minmax`

```erlang
-spec minmax(Target :: enum(), Internalformat :: enum(), Sink :: 0 | 1) -> ok.
```

When `?GL_MINMAX` is enabled, the RGBA components of incoming pixels are
compared to the minimum and maximum values for each component, which are stored
in the two-element minmax table. (The first element stores the minima, and the
second element stores the maxima.) If a pixel component is greater than the
corresponding component in the maximum element, then the maximum element is
updated with the pixel component value. If a pixel component is less than the
corresponding component in the minimum element, then the minimum element is
updated with the pixel component value. (In both cases, if the internal format
of the minmax table includes luminance, then the R color component of incoming
pixels is used for comparison.) The contents of the minmax table may be
retrieved at a later time by calling [`gl:getMinmax/5`](`getMinmax/5`). The
minmax operation is enabled or disabled by calling [`gl:enable/1`](`enable/1`)
or [`gl:disable/1`](`enable/1`), respectively, with an argument of `?GL_MINMAX`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMinmax.xml)

# `minSampleShading`

```erlang
-spec minSampleShading(Value :: f()) -> ok.
```

[`gl:minSampleShading/1`](`minSampleShading/1`) specifies the rate at which
samples are shaded within a covered pixel. Sample-rate shading is enabled by
calling [`gl:enable/1`](`enable/1`) with the parameter `?GL_SAMPLE_SHADING`. If
`?GL_MULTISAMPLE` or `?GL_SAMPLE_SHADING` is disabled, sample shading has no
effect. Otherwise, an implementation must provide at least as many unique color
values for each covered fragment as specified by `Value` times `Samples` where
`Samples` is the value of `?GL_SAMPLES` for the current framebuffer. At least 1
sample for each covered fragment is generated.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMinSampleShading.xhtml)

# `multiDrawArrays`

```erlang
-spec multiDrawArrays(Mode :: enum(), First :: [integer()] | mem(), Count :: [integer()] | mem()) -> ok.
```

[`gl:multiDrawArrays/3`](`multiDrawArrays/3`) specifies multiple sets of
geometric primitives with very few subroutine calls. Instead of calling a GL
procedure to pass each individual vertex, normal, texture coordinate, edge flag,
or color, you can prespecify separate arrays of vertices, normals, and colors
and use them to construct a sequence of primitives with a single call to
[`gl:multiDrawArrays/3`](`multiDrawArrays/3`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawArrays.xhtml)

# `multiDrawArraysIndirect`

```erlang
-spec multiDrawArraysIndirect(Mode :: enum(),
                              Indirect :: offset() | mem(),
                              Drawcount :: i(),
                              Stride :: i()) ->
                                 ok.
```

[`gl:multiDrawArraysIndirect/4`](`multiDrawArraysIndirect/4`) specifies multiple
geometric primitives with very few subroutine calls.
[`gl:multiDrawArraysIndirect/4`](`multiDrawArraysIndirect/4`) behaves similarly
to a multitude of calls to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`),
execept that the parameters to each call to
[`gl:drawArraysInstancedBaseInstance/5`](`drawArraysInstancedBaseInstance/5`)
are stored in an array in memory at the address given by `Indirect`, separated
by the stride, in basic machine units, specified by `Stride`. If `Stride` is
zero, then the array is assumed to be tightly packed in memory.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMultiDrawArraysIndirect.xhtml)

# `multiDrawArraysIndirectCount`

```erlang
-spec multiDrawArraysIndirectCount(Mode, Indirect, Drawcount, Maxdrawcount, Stride) -> ok
                                      when
                                          Mode :: enum(),
                                          Indirect :: offset() | mem(),
                                          Drawcount :: i(),
                                          Maxdrawcount :: i(),
                                          Stride :: i().
```

No documentation available.

# `multiTexCoord1d`

```erlang
-spec multiTexCoord1d(Target :: enum(), S :: f()) -> ok.
```

# `multiTexCoord1dv`

```erlang
-spec multiTexCoord1dv(Target :: enum(), {S :: f()}) -> ok.
```

# `multiTexCoord1f`

```erlang
-spec multiTexCoord1f(Target :: enum(), S :: f()) -> ok.
```

# `multiTexCoord1fv`

```erlang
-spec multiTexCoord1fv(Target :: enum(), {S :: f()}) -> ok.
```

# `multiTexCoord1i`

```erlang
-spec multiTexCoord1i(Target :: enum(), S :: i()) -> ok.
```

# `multiTexCoord1iv`

```erlang
-spec multiTexCoord1iv(Target :: enum(), {S :: i()}) -> ok.
```

# `multiTexCoord1s`

```erlang
-spec multiTexCoord1s(Target :: enum(), S :: i()) -> ok.
```

# `multiTexCoord1sv`

```erlang
-spec multiTexCoord1sv(Target :: enum(), {S :: i()}) -> ok.
```

# `multiTexCoord2d`

```erlang
-spec multiTexCoord2d(Target :: enum(), S :: f(), T :: f()) -> ok.
```

# `multiTexCoord2dv`

```erlang
-spec multiTexCoord2dv(Target :: enum(), {S :: f(), T :: f()}) -> ok.
```

# `multiTexCoord2f`

```erlang
-spec multiTexCoord2f(Target :: enum(), S :: f(), T :: f()) -> ok.
```

# `multiTexCoord2fv`

```erlang
-spec multiTexCoord2fv(Target :: enum(), {S :: f(), T :: f()}) -> ok.
```

# `multiTexCoord2i`

```erlang
-spec multiTexCoord2i(Target :: enum(), S :: i(), T :: i()) -> ok.
```

# `multiTexCoord2iv`

```erlang
-spec multiTexCoord2iv(Target :: enum(), {S :: i(), T :: i()}) -> ok.
```

# `multiTexCoord2s`

```erlang
-spec multiTexCoord2s(Target :: enum(), S :: i(), T :: i()) -> ok.
```

# `multiTexCoord2sv`

```erlang
-spec multiTexCoord2sv(Target :: enum(), {S :: i(), T :: i()}) -> ok.
```

# `multiTexCoord3d`

```erlang
-spec multiTexCoord3d(Target :: enum(), S :: f(), T :: f(), R :: f()) -> ok.
```

# `multiTexCoord3dv`

```erlang
-spec multiTexCoord3dv(Target :: enum(), {S :: f(), T :: f(), R :: f()}) -> ok.
```

# `multiTexCoord3f`

```erlang
-spec multiTexCoord3f(Target :: enum(), S :: f(), T :: f(), R :: f()) -> ok.
```

# `multiTexCoord3fv`

```erlang
-spec multiTexCoord3fv(Target :: enum(), {S :: f(), T :: f(), R :: f()}) -> ok.
```

# `multiTexCoord3i`

```erlang
-spec multiTexCoord3i(Target :: enum(), S :: i(), T :: i(), R :: i()) -> ok.
```

# `multiTexCoord3iv`

```erlang
-spec multiTexCoord3iv(Target :: enum(), {S :: i(), T :: i(), R :: i()}) -> ok.
```

# `multiTexCoord3s`

```erlang
-spec multiTexCoord3s(Target :: enum(), S :: i(), T :: i(), R :: i()) -> ok.
```

# `multiTexCoord3sv`

```erlang
-spec multiTexCoord3sv(Target :: enum(), {S :: i(), T :: i(), R :: i()}) -> ok.
```

# `multiTexCoord4d`

```erlang
-spec multiTexCoord4d(Target :: enum(), S :: f(), T :: f(), R :: f(), Q :: f()) -> ok.
```

# `multiTexCoord4dv`

```erlang
-spec multiTexCoord4dv(Target :: enum(), {S :: f(), T :: f(), R :: f(), Q :: f()}) -> ok.
```

# `multiTexCoord4f`

```erlang
-spec multiTexCoord4f(Target :: enum(), S :: f(), T :: f(), R :: f(), Q :: f()) -> ok.
```

# `multiTexCoord4fv`

```erlang
-spec multiTexCoord4fv(Target :: enum(), {S :: f(), T :: f(), R :: f(), Q :: f()}) -> ok.
```

# `multiTexCoord4i`

```erlang
-spec multiTexCoord4i(Target :: enum(), S :: i(), T :: i(), R :: i(), Q :: i()) -> ok.
```

# `multiTexCoord4iv`

```erlang
-spec multiTexCoord4iv(Target :: enum(), {S :: i(), T :: i(), R :: i(), Q :: i()}) -> ok.
```

# `multiTexCoord4s`

```erlang
-spec multiTexCoord4s(Target :: enum(), S :: i(), T :: i(), R :: i(), Q :: i()) -> ok.
```

# `multiTexCoord4sv`

```erlang
-spec multiTexCoord4sv(Target :: enum(), {S :: i(), T :: i(), R :: i(), Q :: i()}) -> ok.
```

[`gl:multiTexCoord()`](`multiTexCoord1d/2`) specifies texture coordinates in
one, two, three, or four dimensions.
[`gl:multiTexCoord1()`](`multiTexCoord1d/2`) sets the current texture
coordinates to (s 0 0 1); a call to [`gl:multiTexCoord2()`](`multiTexCoord1d/2`)
sets them to (s t 0 1). Similarly, [`gl:multiTexCoord3()`](`multiTexCoord1d/2`)
specifies the texture coordinates as (s t r 1), and
[`gl:multiTexCoord4()`](`multiTexCoord1d/2`) defines all four components
explicitly as (s t r q).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultiTexCoord.xml)

# `multMatrixd`

```erlang
-spec multMatrixd(M :: matrix()) -> ok.
```

# `multMatrixf`

```erlang
-spec multMatrixf(M :: matrix()) -> ok.
```

[`gl:multMatrix()`](`multMatrixd/1`) multiplies the current matrix with the one
specified using `M`, and replaces the current matrix with the product.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultMatrix.xml)

# `multTransposeMatrixd`

```erlang
-spec multTransposeMatrixd(M :: matrix()) -> ok.
```

# `multTransposeMatrixf`

```erlang
-spec multTransposeMatrixf(M :: matrix()) -> ok.
```

[`gl:multTransposeMatrix()`](`multTransposeMatrixf/1`) multiplies the current
matrix with the one specified using `M`, and replaces the current matrix with
the product.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glMultTransposeMatrix.xml)

# `newList`

```erlang
-spec newList(List :: i(), Mode :: enum()) -> ok.
```

Display lists are groups of GL commands that have been stored for subsequent
execution. Display lists are created with [`gl:newList/2`](`newList/2`). All
subsequent commands are placed in the display list, in the order issued, until
[`gl:endList/0`](`newList/2`) is called.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNewList.xml)

# `normal3b`

```erlang
-spec normal3b(Nx :: i(), Ny :: i(), Nz :: i()) -> ok.
```

# `normal3bv`

```erlang
-spec normal3bv({Nx :: i(), Ny :: i(), Nz :: i()}) -> ok.
```

# `normal3d`

```erlang
-spec normal3d(Nx :: f(), Ny :: f(), Nz :: f()) -> ok.
```

# `normal3dv`

```erlang
-spec normal3dv({Nx :: f(), Ny :: f(), Nz :: f()}) -> ok.
```

# `normal3f`

```erlang
-spec normal3f(Nx :: f(), Ny :: f(), Nz :: f()) -> ok.
```

# `normal3fv`

```erlang
-spec normal3fv({Nx :: f(), Ny :: f(), Nz :: f()}) -> ok.
```

# `normal3i`

```erlang
-spec normal3i(Nx :: i(), Ny :: i(), Nz :: i()) -> ok.
```

# `normal3iv`

```erlang
-spec normal3iv({Nx :: i(), Ny :: i(), Nz :: i()}) -> ok.
```

# `normal3s`

```erlang
-spec normal3s(Nx :: i(), Ny :: i(), Nz :: i()) -> ok.
```

# `normal3sv`

```erlang
-spec normal3sv({Nx :: i(), Ny :: i(), Nz :: i()}) -> ok.
```

The current normal is set to the given coordinates whenever
[`gl:normal()`](`normal3b/3`) is issued. Byte, short, or integer arguments are
converted to floating-point format with a linear mapping that maps the most
positive representable integer value to 1.0 and the most negative representable
integer value to -1.0.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormal.xml)

# `normalPointer`

```erlang
-spec normalPointer(Type :: enum(), Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:normalPointer/3`](`normalPointer/3`) specifies the location and data format
of an array of normals to use when rendering. `Type` specifies the data type of
each normal coordinate, and `Stride` specifies the byte stride from one normal
to the next, allowing vertices and attributes to be packed into a single array
or stored in separate arrays. (Single-array storage may be more efficient on
some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glNormalPointer.xml)

# `objectPtrLabel`

```erlang
-spec objectPtrLabel(Ptr :: offset() | mem(), Length :: i(), Label :: string()) -> ok.
```

[`gl:objectPtrLabel/3`](`objectPtrLabel/3`) labels the sync object identified by
`Ptr`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glObjectPtrLabel.xhtml)

# `ortho`

```erlang
-spec ortho(Left :: f(), Right :: f(), Bottom :: f(), Top :: f(), Near_val :: f(), Far_val :: f()) -> ok.
```

[`gl:ortho/6`](`ortho/6`) describes a transformation that produces a parallel
projection. The current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is
multiplied by this matrix and the result replaces the current matrix, as if
[`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix as
its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glOrtho.xml)

# `passThrough`

```erlang
-spec passThrough(Token :: f()) -> ok.
```

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPassThrough.xml)

# `patchParameterfv`

```erlang
-spec patchParameterfv(Pname :: enum(), Values :: [f()]) -> ok.
```

# `patchParameteri`

```erlang
-spec patchParameteri(Pname :: enum(), Value :: i()) -> ok.
```

[`gl:patchParameter()`](`patchParameteri/2`) specifies the parameters that will
be used for patch primitives. `Pname` specifies the parameter to modify and must
be either `?GL_PATCH_VERTICES`, `?GL_PATCH_DEFAULT_OUTER_LEVEL` or
`?GL_PATCH_DEFAULT_INNER_LEVEL`. For
[`gl:patchParameteri/2`](`patchParameteri/2`), `Value` specifies the new value
for the parameter specified by `Pname`. For
[`gl:patchParameterfv/2`](`patchParameteri/2`), `Values` specifies the address
of an array containing the new values for the parameter specified by `Pname`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPatchParameter.xhtml)

# `pauseTransformFeedback`

```erlang
-spec pauseTransformFeedback() -> ok.
```

[`gl:pauseTransformFeedback/0`](`pauseTransformFeedback/0`) pauses transform
feedback operations on the currently active transform feedback object. When
transform feedback operations are paused, transform feedback is still considered
active and changing most transform feedback state related to the object results
in an error. However, a new transform feedback object may be bound while
transform feedback is paused.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPauseTransformFeedback.xhtml)

# `pixelMapfv`

```erlang
-spec pixelMapfv(Map :: enum(), Mapsize :: i(), Values :: binary()) -> ok.
```

# `pixelMapuiv`

```erlang
-spec pixelMapuiv(Map :: enum(), Mapsize :: i(), Values :: binary()) -> ok.
```

# `pixelMapusv`

```erlang
-spec pixelMapusv(Map :: enum(), Mapsize :: i(), Values :: binary()) -> ok.
```

[`gl:pixelMap()`](`pixelMapfv/3`) sets up translation tables, or `maps`, used by
[`gl:copyPixels/5`](`copyPixels/5`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`),
[`gl:drawPixels/5`](`drawPixels/5`), [`gl:readPixels/7`](`readPixels/7`),
[`gl:texImage1D/8`](`texImage1D/8`), [`gl:texImage2D/9`](`texImage2D/9`),
[`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`). Additionally, if the ARB_imaging
subset is supported, the routines [`gl:colorTable/6`](`colorTable/6`),
[`gl:colorSubTable/6`](`colorSubTable/6`),
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`),
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`),
[`gl:histogram/4`](`histogram/4`), [`gl:minmax/3`](`minmax/3`), and
[`gl:separableFilter2D/8`](`separableFilter2D/8`). Use of these maps is
described completely in the [`gl:pixelTransfer()`](`pixelTransferf/2`) reference
page, and partly in the reference pages for the pixel and texture image
commands. Only the specification of the maps is described in this reference
page.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelMap.xml)

# `pixelStoref`

```erlang
-spec pixelStoref(Pname :: enum(), Param :: f()) -> ok.
```

# `pixelStorei`

```erlang
-spec pixelStorei(Pname :: enum(), Param :: i()) -> ok.
```

[`gl:pixelStore()`](`pixelStoref/2`) sets pixel storage modes that affect the
operation of subsequent [`gl:readPixels/7`](`readPixels/7`) as well as the
unpacking of texture patterns (see [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`),
[`gl:texSubImage3D/11`](`texSubImage3D/11`)),
[`gl:compressedTexImage1D/7`](`compressedTexImage1D/7`),
[`gl:compressedTexImage2D/8`](`compressedTexImage2D/8`),
[`gl:compressedTexImage3D/9`](`compressedTexImage3D/9`),
[`gl:compressedTexSubImage1D/7`](`compressedTexSubImage1D/7`),
[`gl:compressedTexSubImage2D/9`](`compressedTexSubImage2D/9`) or
[`gl:compressedTexSubImage1D/7`](`compressedTexSubImage1D/7`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPixelStore.xhtml)

# `pixelTransferf`

```erlang
-spec pixelTransferf(Pname :: enum(), Param :: f()) -> ok.
```

# `pixelTransferi`

```erlang
-spec pixelTransferi(Pname :: enum(), Param :: i()) -> ok.
```

[`gl:pixelTransfer()`](`pixelTransferf/2`) sets pixel transfer modes that affect
the operation of subsequent [`gl:copyPixels/5`](`copyPixels/5`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`),
[`gl:drawPixels/5`](`drawPixels/5`), [`gl:readPixels/7`](`readPixels/7`),
[`gl:texImage1D/8`](`texImage1D/8`), [`gl:texImage2D/9`](`texImage2D/9`),
[`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`) commands. Additionally, if the
ARB_imaging subset is supported, the routines
[`gl:colorTable/6`](`colorTable/6`), [`gl:colorSubTable/6`](`colorSubTable/6`),
[`gl:convolutionFilter1D/6`](`convolutionFilter1D/6`),
[`gl:convolutionFilter2D/7`](`convolutionFilter2D/7`),
[`gl:histogram/4`](`histogram/4`), [`gl:minmax/3`](`minmax/3`), and
[`gl:separableFilter2D/8`](`separableFilter2D/8`) are also affected. The
algorithms that are specified by pixel transfer modes operate on pixels after
they are read from the frame buffer
([`gl:copyPixels/5`](`copyPixels/5`)[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`),
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`), and
[`gl:readPixels/7`](`readPixels/7`)), or unpacked from client memory
([`gl:drawPixels/5`](`drawPixels/5`), [`gl:texImage1D/8`](`texImage1D/8`),
[`gl:texImage2D/9`](`texImage2D/9`), [`gl:texImage3D/10`](`texImage3D/10`),
[`gl:texSubImage1D/7`](`texSubImage1D/7`),
[`gl:texSubImage2D/9`](`texSubImage2D/9`), and
[`gl:texSubImage3D/11`](`texSubImage3D/11`)). Pixel transfer operations happen
in the same order, and in the same manner, regardless of the command that
resulted in the pixel operation. Pixel storage modes (see
[`gl:pixelStore()`](`pixelStoref/2`)) control the unpacking of pixels being read
from client memory and the packing of pixels being written back into client
memory.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelTransfer.xml)

# `pixelZoom`

```erlang
-spec pixelZoom(Xfactor :: f(), Yfactor :: f()) -> ok.
```

[`gl:pixelZoom/2`](`pixelZoom/2`) specifies values for the x and y zoom factors.
During the execution of [`gl:drawPixels/5`](`drawPixels/5`) or
[`gl:copyPixels/5`](`copyPixels/5`), if ( xr, yr) is the current raster
position, and a given element is in the mth row and nth column of the pixel
rectangle, then pixels whose centers are in the rectangle with corners at

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPixelZoom.xml)

# `pointParameterf`

```erlang
-spec pointParameterf(Pname :: enum(), Param :: f()) -> ok.
```

# `pointParameterfv`

```erlang
-spec pointParameterfv(Pname :: enum(), Params :: tuple()) -> ok.
```

# `pointParameteri`

```erlang
-spec pointParameteri(Pname :: enum(), Param :: i()) -> ok.
```

# `pointParameteriv`

```erlang
-spec pointParameteriv(Pname :: enum(), Params :: tuple()) -> ok.
```

The following values are accepted for `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointParameter.xhtml)

# `pointSize`

```erlang
-spec pointSize(Size :: f()) -> ok.
```

[`gl:pointSize/1`](`pointSize/1`) specifies the rasterized diameter of points.
If point size mode is disabled (see [`gl:enable/1`](`enable/1`) with parameter
`?GL_PROGRAM_POINT_SIZE`), this value will be used to rasterize points.
Otherwise, the value written to the shading language built-in variable
gl_PointSize will be used.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPointSize.xhtml)

# `polygonMode`

```erlang
-spec polygonMode(Face :: enum(), Mode :: enum()) -> ok.
```

[`gl:polygonMode/2`](`polygonMode/2`) controls the interpretation of polygons
for rasterization. `Face` describes which polygons `Mode` applies to: both front
and back-facing polygons (`?GL_FRONT_AND_BACK`). The polygon mode affects only
the final rasterization of polygons. In particular, a polygon's vertices are lit
and the polygon is clipped and possibly culled before these modes are applied.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonMode.xhtml)

# `polygonOffset`

```erlang
-spec polygonOffset(Factor :: f(), Units :: f()) -> ok.
```

When `?GL_POLYGON_OFFSET_FILL`, `?GL_POLYGON_OFFSET_LINE`, or
`?GL_POLYGON_OFFSET_POINT` is enabled, each fragment's `depth` value will be
offset after it is interpolated from the `depth` values of the appropriate
vertices. The value of the offset is factor×DZ+r×units, where DZ is a
measurement of the change in depth relative to the screen area of the polygon,
and r is the smallest value that is guaranteed to produce a resolvable offset
for a given implementation. The offset is added before the depth test is
performed and before the value is written into the depth buffer.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonOffset.xhtml)

# `polygonOffsetClamp`

```erlang
-spec polygonOffsetClamp(Factor :: f(), Units :: f(), Clamp :: f()) -> ok.
```

No documentation available.

# `polygonStipple`

```erlang
-spec polygonStipple(Mask :: binary()) -> ok.
```

Polygon stippling, like line stippling (see
[`gl:lineStipple/2`](`lineStipple/2`)), masks out certain fragments produced by
rasterization, creating a pattern. Stippling is independent of polygon
antialiasing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPolygonStipple.xml)

# `popAttrib`

```erlang
-spec popAttrib() -> ok.
```

# `popClientAttrib`

```erlang
-spec popClientAttrib() -> ok.
```

# `popDebugGroup`

```erlang
-spec popDebugGroup() -> ok.
```

# `popMatrix`

```erlang
-spec popMatrix() -> ok.
```

# `popName`

```erlang
-spec popName() -> ok.
```

# `primitiveRestartIndex`

```erlang
-spec primitiveRestartIndex(Index :: i()) -> ok.
```

[`gl:primitiveRestartIndex/1`](`primitiveRestartIndex/1`) specifies a vertex
array element that is treated specially when primitive restarting is enabled.
This is known as the primitive restart index.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPrimitiveRestartIndex.xhtml)

# `prioritizeTextures`

```erlang
-spec prioritizeTextures(Textures :: [i()], Priorities :: [clamp()]) -> ok.
```

[`gl:prioritizeTextures/2`](`prioritizeTextures/2`) assigns the `N` texture
priorities given in `Priorities` to the `N` textures named in `Textures`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPrioritizeTextures.xml)

# `programBinary`

```erlang
-spec programBinary(Program :: i(), BinaryFormat :: enum(), Binary :: binary()) -> ok.
```

[`gl:programBinary/3`](`programBinary/3`) loads a program object with a program
binary previously returned from [`gl:getProgramBinary/2`](`getProgramBinary/2`).
`BinaryFormat` and `Binary` must be those returned by a previous call to
[`gl:getProgramBinary/2`](`getProgramBinary/2`), and `Length` must be the length
returned by [`gl:getProgramBinary/2`](`getProgramBinary/2`), or by
[`gl:getProgram()`](`getProgramiv/2`) when called with `Pname` set to
`?GL_PROGRAM_BINARY_LENGTH`. If these conditions are not met, loading the
program binary will fail and `Program`'s `?GL_LINK_STATUS` will be set to
`?GL_FALSE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramBinary.xhtml)

# `programParameteri`

```erlang
-spec programParameteri(Program :: i(), Pname :: enum(), Value :: i()) -> ok.
```

[`gl:programParameter()`](`programParameteri/3`) specifies a new value for the
parameter nameed by `Pname` for the program object `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramParameter.xhtml)

# `programUniform1d`

```erlang
-spec programUniform1d(Program :: i(), Location :: i(), V0 :: f()) -> ok.
```

# `programUniform1dv`

```erlang
-spec programUniform1dv(Program :: i(), Location :: i(), Value :: [f()]) -> ok.
```

# `programUniform1f`

```erlang
-spec programUniform1f(Program :: i(), Location :: i(), V0 :: f()) -> ok.
```

# `programUniform1fv`

```erlang
-spec programUniform1fv(Program :: i(), Location :: i(), Value :: [f()]) -> ok.
```

# `programUniform1i`

```erlang
-spec programUniform1i(Program :: i(), Location :: i(), V0 :: i()) -> ok.
```

# `programUniform1iv`

```erlang
-spec programUniform1iv(Program :: i(), Location :: i(), Value :: [i()]) -> ok.
```

# `programUniform1ui`

```erlang
-spec programUniform1ui(Program :: i(), Location :: i(), V0 :: i()) -> ok.
```

# `programUniform1uiv`

```erlang
-spec programUniform1uiv(Program :: i(), Location :: i(), Value :: [i()]) -> ok.
```

# `programUniform2d`

```erlang
-spec programUniform2d(Program :: i(), Location :: i(), V0 :: f(), V1 :: f()) -> ok.
```

# `programUniform2dv`

```erlang
-spec programUniform2dv(Program :: i(), Location :: i(), Value :: [{f(), f()}]) -> ok.
```

# `programUniform2f`

```erlang
-spec programUniform2f(Program :: i(), Location :: i(), V0 :: f(), V1 :: f()) -> ok.
```

# `programUniform2fv`

```erlang
-spec programUniform2fv(Program :: i(), Location :: i(), Value :: [{f(), f()}]) -> ok.
```

# `programUniform2i`

```erlang
-spec programUniform2i(Program :: i(), Location :: i(), V0 :: i(), V1 :: i()) -> ok.
```

# `programUniform2iv`

```erlang
-spec programUniform2iv(Program :: i(), Location :: i(), Value :: [{i(), i()}]) -> ok.
```

# `programUniform2ui`

```erlang
-spec programUniform2ui(Program :: i(), Location :: i(), V0 :: i(), V1 :: i()) -> ok.
```

# `programUniform2uiv`

```erlang
-spec programUniform2uiv(Program :: i(), Location :: i(), Value :: [{i(), i()}]) -> ok.
```

# `programUniform3d`

```erlang
-spec programUniform3d(Program :: i(), Location :: i(), V0 :: f(), V1 :: f(), V2 :: f()) -> ok.
```

# `programUniform3dv`

```erlang
-spec programUniform3dv(Program :: i(), Location :: i(), Value :: [{f(), f(), f()}]) -> ok.
```

# `programUniform3f`

```erlang
-spec programUniform3f(Program :: i(), Location :: i(), V0 :: f(), V1 :: f(), V2 :: f()) -> ok.
```

# `programUniform3fv`

```erlang
-spec programUniform3fv(Program :: i(), Location :: i(), Value :: [{f(), f(), f()}]) -> ok.
```

# `programUniform3i`

```erlang
-spec programUniform3i(Program :: i(), Location :: i(), V0 :: i(), V1 :: i(), V2 :: i()) -> ok.
```

# `programUniform3iv`

```erlang
-spec programUniform3iv(Program :: i(), Location :: i(), Value :: [{i(), i(), i()}]) -> ok.
```

# `programUniform3ui`

```erlang
-spec programUniform3ui(Program :: i(), Location :: i(), V0 :: i(), V1 :: i(), V2 :: i()) -> ok.
```

# `programUniform3uiv`

```erlang
-spec programUniform3uiv(Program :: i(), Location :: i(), Value :: [{i(), i(), i()}]) -> ok.
```

# `programUniform4d`

```erlang
-spec programUniform4d(Program :: i(), Location :: i(), V0 :: f(), V1 :: f(), V2 :: f(), V3 :: f()) ->
                          ok.
```

# `programUniform4dv`

```erlang
-spec programUniform4dv(Program :: i(), Location :: i(), Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `programUniform4f`

```erlang
-spec programUniform4f(Program :: i(), Location :: i(), V0 :: f(), V1 :: f(), V2 :: f(), V3 :: f()) ->
                          ok.
```

# `programUniform4fv`

```erlang
-spec programUniform4fv(Program :: i(), Location :: i(), Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `programUniform4i`

```erlang
-spec programUniform4i(Program :: i(), Location :: i(), V0 :: i(), V1 :: i(), V2 :: i(), V3 :: i()) ->
                          ok.
```

# `programUniform4iv`

```erlang
-spec programUniform4iv(Program :: i(), Location :: i(), Value :: [{i(), i(), i(), i()}]) -> ok.
```

# `programUniform4ui`

```erlang
-spec programUniform4ui(Program :: i(), Location :: i(), V0 :: i(), V1 :: i(), V2 :: i(), V3 :: i()) ->
                           ok.
```

# `programUniform4uiv`

```erlang
-spec programUniform4uiv(Program :: i(), Location :: i(), Value :: [{i(), i(), i(), i()}]) -> ok.
```

# `programUniformMatrix2dv`

```erlang
-spec programUniformMatrix2dv(Program :: i(),
                              Location :: i(),
                              Transpose :: 0 | 1,
                              Value :: [{f(), f(), f(), f()}]) ->
                                 ok.
```

# `programUniformMatrix2fv`

```erlang
-spec programUniformMatrix2fv(Program :: i(),
                              Location :: i(),
                              Transpose :: 0 | 1,
                              Value :: [{f(), f(), f(), f()}]) ->
                                 ok.
```

# `programUniformMatrix2x3dv`

```erlang
-spec programUniformMatrix2x3dv(Program :: i(),
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                                   ok.
```

# `programUniformMatrix2x3fv`

```erlang
-spec programUniformMatrix2x3fv(Program :: i(),
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                                   ok.
```

# `programUniformMatrix2x4dv`

```erlang
-spec programUniformMatrix2x4dv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix2x4fv`

```erlang
-spec programUniformMatrix2x4fv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix3dv`

```erlang
-spec programUniformMatrix3dv(Program, Location, Transpose, Value) -> ok
                                 when
                                     Program :: i(),
                                     Location :: i(),
                                     Transpose :: 0 | 1,
                                     Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix3fv`

```erlang
-spec programUniformMatrix3fv(Program, Location, Transpose, Value) -> ok
                                 when
                                     Program :: i(),
                                     Location :: i(),
                                     Transpose :: 0 | 1,
                                     Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix3x2dv`

```erlang
-spec programUniformMatrix3x2dv(Program :: i(),
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                                   ok.
```

# `programUniformMatrix3x2fv`

```erlang
-spec programUniformMatrix3x2fv(Program :: i(),
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                                   ok.
```

# `programUniformMatrix3x4dv`

```erlang
-spec programUniformMatrix3x4dv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value ::
                                           [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix3x4fv`

```erlang
-spec programUniformMatrix3x4fv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value ::
                                           [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix4dv`

```erlang
-spec programUniformMatrix4dv(Program, Location, Transpose, Value) -> ok
                                 when
                                     Program :: i(),
                                     Location :: i(),
                                     Transpose :: 0 | 1,
                                     Value ::
                                         [{f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f()}].
```

# `programUniformMatrix4fv`

```erlang
-spec programUniformMatrix4fv(Program, Location, Transpose, Value) -> ok
                                 when
                                     Program :: i(),
                                     Location :: i(),
                                     Transpose :: 0 | 1,
                                     Value ::
                                         [{f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f(),
                                           f()}].
```

# `programUniformMatrix4x2dv`

```erlang
-spec programUniformMatrix4x2dv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix4x2fv`

```erlang
-spec programUniformMatrix4x2fv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix4x3dv`

```erlang
-spec programUniformMatrix4x3dv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value ::
                                           [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `programUniformMatrix4x3fv`

```erlang
-spec programUniformMatrix4x3fv(Program, Location, Transpose, Value) -> ok
                                   when
                                       Program :: i(),
                                       Location :: i(),
                                       Transpose :: 0 | 1,
                                       Value ::
                                           [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

[`gl:programUniform()`](`programUniform1i/3`) modifies the value of a uniform
variable or a uniform variable array. The location of the uniform variable to be
modified is specified by `Location`, which should be a value returned by
[`gl:getUniformLocation/2`](`getUniformLocation/2`).
[`gl:programUniform()`](`programUniform1i/3`) operates on the program object
specified by `Program`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProgramUniform.xhtml)

# `provokingVertex`

```erlang
-spec provokingVertex(Mode :: enum()) -> ok.
```

`Flatshading` a vertex shader varying output means to assign all vetices of the
primitive the same value for that output. The vertex from which these values is
derived is known as the `provoking vertex` and
[`gl:provokingVertex/1`](`provokingVertex/1`) specifies which vertex is to be
used as the source of data for flat shaded varyings.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glProvokingVertex.xhtml)

# `pushAttrib`

```erlang
-spec pushAttrib(Mask :: i()) -> ok.
```

[`gl:pushAttrib/1`](`pushAttrib/1`) takes one argument, a mask that indicates
which groups of state variables to save on the attribute stack. Symbolic
constants are used to set bits in the mask. `Mask` is typically constructed by
specifying the bitwise-or of several of these constants together. The special
mask `?GL_ALL_ATTRIB_BITS` can be used to save all stackable states.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushAttrib.xml)

# `pushClientAttrib`

```erlang
-spec pushClientAttrib(Mask :: i()) -> ok.
```

[`gl:pushClientAttrib/1`](`pushClientAttrib/1`) takes one argument, a mask that
indicates which groups of client-state variables to save on the client attribute
stack. Symbolic constants are used to set bits in the mask. `Mask` is typically
constructed by specifying the bitwise-or of several of these constants together.
The special mask `?GL_CLIENT_ALL_ATTRIB_BITS` can be used to save all stackable
client state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushClientAttrib.xml)

# `pushDebugGroup`

```erlang
-spec pushDebugGroup(Source :: enum(), Id :: i(), Length :: i(), Message :: string()) -> ok.
```

[`gl:pushDebugGroup/4`](`pushDebugGroup/4`) pushes a debug group described by
the string `Message` into the command stream. The value of `Id` specifies the ID
of messages generated. The parameter `Length` contains the number of characters
in `Message`. If `Length` is negative, it is implied that `Message` contains a
null terminated string. The message has the specified `Source` and `Id`, the
`Type``?GL_DEBUG_TYPE_PUSH_GROUP`, and
`Severity``?GL_DEBUG_SEVERITY_NOTIFICATION`. The GL will put a new debug group
on top of the debug group stack which inherits the control of the volume of
debug output of the debug group previously residing on the top of the debug
group stack. Because debug groups are strictly hierarchical, any additional
control of the debug output volume will only apply within the active debug group
and the debug groups pushed on top of the active debug group.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPushDebugGroup.xhtml)

# `pushMatrix`

```erlang
-spec pushMatrix() -> ok.
```

There is a stack of matrices for each of the matrix modes. In `?GL_MODELVIEW`
mode, the stack depth is at least 32. In the other modes, `?GL_COLOR`,
`?GL_PROJECTION`, and `?GL_TEXTURE`, the depth is at least 2. The current matrix
in any mode is the matrix on the top of the stack for that mode.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushMatrix.xml)

# `pushName`

```erlang
-spec pushName(Name :: i()) -> ok.
```

The name stack is used during selection mode to allow sets of rendering commands
to be uniquely identified. It consists of an ordered set of unsigned integers
and is initially empty.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glPushName.xml)

# `queryCounter`

```erlang
-spec queryCounter(Id :: i(), Target :: enum()) -> ok.
```

[`gl:queryCounter/2`](`queryCounter/2`) causes the GL to record the current time
into the query object named `Id`. `Target` must be `?GL_TIMESTAMP`. The time is
recorded after all previous commands on the GL client and server state and the
framebuffer have been fully realized. When the time is recorded, the query
result for that object is marked available.
[`gl:queryCounter/2`](`queryCounter/2`) timer queries can be used within a
[`gl:beginQuery/2`](`beginQuery/2`) / [`gl:endQuery/1`](`beginQuery/2`) block
where the target is `?GL_TIME_ELAPSED` and it does not affect the result of that
query object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glQueryCounter.xhtml)

# `rasterPos2d`

```erlang
-spec rasterPos2d(X :: f(), Y :: f()) -> ok.
```

# `rasterPos2dv`

```erlang
-spec rasterPos2dv({X :: f(), Y :: f()}) -> ok.
```

# `rasterPos2f`

```erlang
-spec rasterPos2f(X :: f(), Y :: f()) -> ok.
```

# `rasterPos2fv`

```erlang
-spec rasterPos2fv({X :: f(), Y :: f()}) -> ok.
```

# `rasterPos2i`

```erlang
-spec rasterPos2i(X :: i(), Y :: i()) -> ok.
```

# `rasterPos2iv`

```erlang
-spec rasterPos2iv({X :: i(), Y :: i()}) -> ok.
```

# `rasterPos2s`

```erlang
-spec rasterPos2s(X :: i(), Y :: i()) -> ok.
```

# `rasterPos2sv`

```erlang
-spec rasterPos2sv({X :: i(), Y :: i()}) -> ok.
```

# `rasterPos3d`

```erlang
-spec rasterPos3d(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `rasterPos3dv`

```erlang
-spec rasterPos3dv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `rasterPos3f`

```erlang
-spec rasterPos3f(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `rasterPos3fv`

```erlang
-spec rasterPos3fv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `rasterPos3i`

```erlang
-spec rasterPos3i(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `rasterPos3iv`

```erlang
-spec rasterPos3iv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `rasterPos3s`

```erlang
-spec rasterPos3s(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `rasterPos3sv`

```erlang
-spec rasterPos3sv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `rasterPos4d`

```erlang
-spec rasterPos4d(X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `rasterPos4dv`

```erlang
-spec rasterPos4dv({X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `rasterPos4f`

```erlang
-spec rasterPos4f(X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `rasterPos4fv`

```erlang
-spec rasterPos4fv({X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `rasterPos4i`

```erlang
-spec rasterPos4i(X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `rasterPos4iv`

```erlang
-spec rasterPos4iv({X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `rasterPos4s`

```erlang
-spec rasterPos4s(X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `rasterPos4sv`

```erlang
-spec rasterPos4sv({X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

The GL maintains a 3D position in window coordinates. This position, called the
raster position, is used to position pixel and bitmap write operations. It is
maintained with subpixel accuracy. See [`gl:bitmap/7`](`bitmap/7`),
[`gl:drawPixels/5`](`drawPixels/5`), and [`gl:copyPixels/5`](`copyPixels/5`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRasterPos.xml)

# `readBuffer`

```erlang
-spec readBuffer(Mode :: enum()) -> ok.
```

[`gl:readBuffer/1`](`readBuffer/1`) specifies a color buffer as the source for
subsequent [`gl:readPixels/7`](`readPixels/7`),
[`gl:copyTexImage1D/7`](`copyTexImage1D/7`),
[`gl:copyTexImage2D/8`](`copyTexImage2D/8`),
[`gl:copyTexSubImage1D/6`](`copyTexSubImage1D/6`),
[`gl:copyTexSubImage2D/8`](`copyTexSubImage2D/8`), and
[`gl:copyTexSubImage3D/9`](`copyTexSubImage3D/9`) commands. `Mode` accepts one
of twelve or more predefined values. In a fully configured system, `?GL_FRONT`,
`?GL_LEFT`, and `?GL_FRONT_LEFT` all name the front left buffer,
`?GL_FRONT_RIGHT` and `?GL_RIGHT` name the front right buffer, and
`?GL_BACK_LEFT` and `?GL_BACK` name the back left buffer. Further more, the
constants `?GL_COLOR_ATTACHMENT``i` may be used to indicate the `i`th color
attachment where `i` ranges from zero to the value of
`?GL_MAX_COLOR_ATTACHMENTS` minus one.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadBuffer.xhtml)

# `readPixels`

```erlang
-spec readPixels(X, Y, Width, Height, Format, Type, Pixels) -> ok
                    when
                        X :: i(),
                        Y :: i(),
                        Width :: i(),
                        Height :: i(),
                        Format :: enum(),
                        Type :: enum(),
                        Pixels :: mem().
```

[`gl:readPixels/7`](`readPixels/7`) and `glReadnPixels` return pixel data from
the frame buffer, starting with the pixel whose lower left corner is at location
(`X`, `Y`), into client memory starting at location `Data`. Several parameters
control the processing of the pixel data before it is placed into client memory.
These parameters are set with [`gl:pixelStore()`](`pixelStoref/2`). This
reference page describes the effects on [`gl:readPixels/7`](`readPixels/7`) and
`glReadnPixels` of most, but not all of the parameters specified by these three
commands.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml)

# `rectd`

```erlang
-spec rectd(X1 :: f(), Y1 :: f(), X2 :: f(), Y2 :: f()) -> ok.
```

# `rectdv`

```erlang
-spec rectdv(V1 :: {f(), f()}, V2 :: {f(), f()}) -> ok.
```

# `rectf`

```erlang
-spec rectf(X1 :: f(), Y1 :: f(), X2 :: f(), Y2 :: f()) -> ok.
```

# `rectfv`

```erlang
-spec rectfv(V1 :: {f(), f()}, V2 :: {f(), f()}) -> ok.
```

# `recti`

```erlang
-spec recti(X1 :: i(), Y1 :: i(), X2 :: i(), Y2 :: i()) -> ok.
```

# `rectiv`

```erlang
-spec rectiv(V1 :: {i(), i()}, V2 :: {i(), i()}) -> ok.
```

# `rects`

```erlang
-spec rects(X1 :: i(), Y1 :: i(), X2 :: i(), Y2 :: i()) -> ok.
```

# `rectsv`

```erlang
-spec rectsv(V1 :: {i(), i()}, V2 :: {i(), i()}) -> ok.
```

[`gl:rect()`](`rectd/4`) supports efficient specification of rectangles as two
corner points. Each rectangle command takes four arguments, organized either as
two consecutive pairs of (x y) coordinates or as two pointers to arrays, each
containing an (x y) pair. The resulting rectangle is defined in the z=0 plane.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRect.xml)

# `releaseShaderCompiler`

```erlang
-spec releaseShaderCompiler() -> ok.
```

[`gl:releaseShaderCompiler/0`](`releaseShaderCompiler/0`) provides a hint to the
implementation that it may free internal resources associated with its shader
compiler. [`gl:compileShader/1`](`compileShader/1`) may subsequently be called
and the implementation may at that time reallocate resources previously freed by
the call to [`gl:releaseShaderCompiler/0`](`releaseShaderCompiler/0`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReleaseShaderCompiler.xhtml)

# `renderbufferStorage`

```erlang
-spec renderbufferStorage(Target :: enum(), Internalformat :: enum(), Width :: i(), Height :: i()) -> ok.
```

[`gl:renderbufferStorage/4`](`renderbufferStorage/4`) is equivalent to calling
[`gl:renderbufferStorageMultisample/5`](`renderbufferStorageMultisample/5`) with
the `Samples` set to zero, and `glNamedRenderbufferStorage` is equivalent to
calling `glNamedRenderbufferStorageMultisample` with the samples set to zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorage.xhtml)

# `renderbufferStorageMultisample`

```erlang
-spec renderbufferStorageMultisample(Target :: enum(),
                                     Samples :: i(),
                                     Internalformat :: enum(),
                                     Width :: i(),
                                     Height :: i()) ->
                                        ok.
```

[`gl:renderbufferStorageMultisample/5`](`renderbufferStorageMultisample/5`) and
`glNamedRenderbufferStorageMultisample` establish the data storage, format,
dimensions and number of samples of a renderbuffer object's image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glRenderbufferStorageMultisample.xhtml)

# `renderMode`

```erlang
-spec renderMode(Mode :: enum()) -> i().
```

[`gl:renderMode/1`](`renderMode/1`) sets the rasterization mode. It takes one
argument, `Mode`, which can assume one of three predefined values:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRenderMode.xml)

# `resetHistogram`

```erlang
-spec resetHistogram(Target :: enum()) -> ok.
```

[`gl:resetHistogram/1`](`resetHistogram/1`) resets all the elements of the
current histogram table to zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetHistogram.xml)

# `resetMinmax`

```erlang
-spec resetMinmax(Target :: enum()) -> ok.
```

[`gl:resetMinmax/1`](`resetMinmax/1`) resets the elements of the current minmax
table to their initial values: the \`\`maximum'' element receives the minimum
possible component values, and the \`\`minimum'' element receives the maximum
possible component values.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glResetMinmax.xml)

# `resumeTransformFeedback`

```erlang
-spec resumeTransformFeedback() -> ok.
```

[`gl:resumeTransformFeedback/0`](`resumeTransformFeedback/0`) resumes transform
feedback operations on the currently active transform feedback object. When
transform feedback operations are paused, transform feedback is still considered
active and changing most transform feedback state related to the object results
in an error. However, a new transform feedback object may be bound while
transform feedback is paused.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glResumeTransformFeedback.xhtml)

# `rotated`

```erlang
-spec rotated(Angle :: f(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `rotatef`

```erlang
-spec rotatef(Angle :: f(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

[`gl:rotate()`](`rotated/4`) produces a rotation of `Angle` degrees around the
vector (x y z). The current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is
multiplied by a rotation matrix with the product replacing the current matrix,
as if [`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix
as its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glRotate.xml)

# `sampleCoverage`

```erlang
-spec sampleCoverage(Value :: clamp(), Invert :: 0 | 1) -> ok.
```

Multisampling samples a pixel multiple times at various implementation-dependent
subpixel locations to generate antialiasing effects. Multisampling transparently
antialiases points, lines, polygons, and images if it is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleCoverage.xhtml)

# `sampleMaski`

```erlang
-spec sampleMaski(MaskNumber :: i(), Mask :: i()) -> ok.
```

[`gl:sampleMaski/2`](`sampleMaski/2`) sets one 32-bit sub-word of the multi-word
sample mask, `?GL_SAMPLE_MASK_VALUE`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSampleMaski.xhtml)

# `samplerParameterf`

```erlang
-spec samplerParameterf(Sampler :: i(), Pname :: enum(), Param :: f()) -> ok.
```

# `samplerParameterfv`

```erlang
-spec samplerParameterfv(Sampler :: i(), Pname :: enum(), Param :: [f()]) -> ok.
```

# `samplerParameterIiv`

```erlang
-spec samplerParameterIiv(Sampler :: i(), Pname :: enum(), Param :: [i()]) -> ok.
```

# `samplerParameterIuiv`

```erlang
-spec samplerParameterIuiv(Sampler :: i(), Pname :: enum(), Param :: [i()]) -> ok.
```

# `samplerParameteri`

```erlang
-spec samplerParameteri(Sampler :: i(), Pname :: enum(), Param :: i()) -> ok.
```

# `samplerParameteriv`

```erlang
-spec samplerParameteriv(Sampler :: i(), Pname :: enum(), Param :: [i()]) -> ok.
```

[`gl:samplerParameter()`](`samplerParameteri/3`) assigns the value or values in
`Params` to the sampler parameter specified as `Pname`. `Sampler` specifies the
sampler object to be modified, and must be the name of a sampler object
previously returned from a call to [`gl:genSamplers/1`](`genSamplers/1`). The
following symbols are accepted in `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glSamplerParameter.xhtml)

# `scaled`

```erlang
-spec scaled(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `scalef`

```erlang
-spec scalef(X :: f(), Y :: f(), Z :: f()) -> ok.
```

[`gl:scale()`](`scaled/3`) produces a nonuniform scaling along the `x`, `y`, and
`z` axes. The three parameters indicate the desired scale factor along each of
the three axes.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glScale.xml)

# `scissor`

```erlang
-spec scissor(X :: i(), Y :: i(), Width :: i(), Height :: i()) -> ok.
```

[`gl:scissor/4`](`scissor/4`) defines a rectangle, called the scissor box, in
window coordinates. The first two arguments, `X` and `Y`, specify the lower left
corner of the box. `Width` and `Height` specify the width and height of the box.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissor.xhtml)

# `scissorArrayv`

```erlang
-spec scissorArrayv(First :: i(), V :: [{i(), i(), i(), i()}]) -> ok.
```

[`gl:scissorArrayv/2`](`scissorArrayv/2`) defines rectangles, called scissor
boxes, in window coordinates for each viewport. `First` specifies the index of
the first scissor box to modify and `Count` specifies the number of scissor
boxes to modify. `First` must be less than the value of `?GL_MAX_VIEWPORTS`, and
`First` \+ `Count` must be less than or equal to the value of
`?GL_MAX_VIEWPORTS`. `V` specifies the address of an array containing integers
specifying the lower left corner of the scissor boxes, and the width and height
of the scissor boxes, in that order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissorArray.xhtml)

# `scissorIndexed`

```erlang
-spec scissorIndexed(Index :: i(), Left :: i(), Bottom :: i(), Width :: i(), Height :: i()) -> ok.
```

# `scissorIndexedv`

```erlang
-spec scissorIndexedv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

[`gl:scissorIndexed/5`](`scissorIndexed/5`) defines the scissor box for a
specified viewport. `Index` specifies the index of scissor box to modify.
`Index` must be less than the value of `?GL_MAX_VIEWPORTS`. For
[`gl:scissorIndexed/5`](`scissorIndexed/5`), `Left`, `Bottom`, `Width` and
`Height` specify the left, bottom, width and height of the scissor box, in
pixels, respectively. For [`gl:scissorIndexedv/2`](`scissorIndexed/5`), `V`
specifies the address of an array containing integers specifying the lower left
corner of the scissor box, and the width and height of the scissor box, in that
order.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glScissorIndexed.xhtml)

# `secondaryColor3b`

```erlang
-spec secondaryColor3b(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3bv`

```erlang
-spec secondaryColor3bv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `secondaryColor3d`

```erlang
-spec secondaryColor3d(Red :: f(), Green :: f(), Blue :: f()) -> ok.
```

# `secondaryColor3dv`

```erlang
-spec secondaryColor3dv({Red :: f(), Green :: f(), Blue :: f()}) -> ok.
```

# `secondaryColor3f`

```erlang
-spec secondaryColor3f(Red :: f(), Green :: f(), Blue :: f()) -> ok.
```

# `secondaryColor3fv`

```erlang
-spec secondaryColor3fv({Red :: f(), Green :: f(), Blue :: f()}) -> ok.
```

# `secondaryColor3i`

```erlang
-spec secondaryColor3i(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3iv`

```erlang
-spec secondaryColor3iv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `secondaryColor3s`

```erlang
-spec secondaryColor3s(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3sv`

```erlang
-spec secondaryColor3sv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `secondaryColor3ub`

```erlang
-spec secondaryColor3ub(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3ubv`

```erlang
-spec secondaryColor3ubv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `secondaryColor3ui`

```erlang
-spec secondaryColor3ui(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3uiv`

```erlang
-spec secondaryColor3uiv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

# `secondaryColor3us`

```erlang
-spec secondaryColor3us(Red :: i(), Green :: i(), Blue :: i()) -> ok.
```

# `secondaryColor3usv`

```erlang
-spec secondaryColor3usv({Red :: i(), Green :: i(), Blue :: i()}) -> ok.
```

The GL stores both a primary four-valued RGBA color and a secondary four-valued
RGBA color (where alpha is always set to 0.0) that is associated with every
vertex.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColor.xml)

# `secondaryColorPointer`

```erlang
-spec secondaryColorPointer(Size :: i(), Type :: enum(), Stride :: i(), Pointer :: offset() | mem()) ->
                               ok.
```

[`gl:secondaryColorPointer/4`](`secondaryColorPointer/4`) specifies the location
and data format of an array of color components to use when rendering. `Size`
specifies the number of components per color, and must be 3. `Type` specifies
the data type of each color component, and `Stride` specifies the byte stride
from one color to the next, allowing vertices and attributes to be packed into a
single array or stored in separate arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSecondaryColorPointer.xml)

# `selectBuffer`

```erlang
-spec selectBuffer(Size :: i(), Buffer :: mem()) -> ok.
```

[`gl:selectBuffer/2`](`selectBuffer/2`) has two arguments: `Buffer` is a pointer
to an array of unsigned integers, and `Size` indicates the size of the array.
`Buffer` returns values from the name stack (see
[`gl:initNames/0`](`initNames/0`), [`gl:loadName/1`](`loadName/1`),
[`gl:pushName/1`](`pushName/1`)) when the rendering mode is `?GL_SELECT` (see
[`gl:renderMode/1`](`renderMode/1`)). [`gl:selectBuffer/2`](`selectBuffer/2`)
must be issued before selection mode is enabled, and it must not be issued while
the rendering mode is `?GL_SELECT`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSelectBuffer.xml)

# `separableFilter2D`

```erlang
-spec separableFilter2D(Target, Internalformat, Width, Height, Format, Type, Row, Column) -> ok
                           when
                               Target :: enum(),
                               Internalformat :: enum(),
                               Width :: i(),
                               Height :: i(),
                               Format :: enum(),
                               Type :: enum(),
                               Row :: offset() | mem(),
                               Column :: offset() | mem().
```

[`gl:separableFilter2D/8`](`separableFilter2D/8`) builds a two-dimensional
separable convolution filter kernel from two arrays of pixels.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glSeparableFilter2D.xml)

# `shadeModel`

```erlang
-spec shadeModel(Mode :: enum()) -> ok.
```

GL primitives can have either flat or smooth shading. Smooth shading, the
default, causes the computed colors of vertices to be interpolated as the
primitive is rasterized, typically assigning different colors to each resulting
pixel fragment. Flat shading selects the computed color of just one vertex and
assigns it to all the pixel fragments generated by rasterizing a single
primitive. In either case, the computed color of a vertex is the result of
lighting if lighting is enabled, or it is the current color at the time the
vertex was specified if lighting is disabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glShadeModel.xml)

# `shaderBinary`

```erlang
-spec shaderBinary(Shaders :: [i()], Binaryformat :: enum(), Binary :: binary()) -> ok.
```

[`gl:shaderBinary/3`](`shaderBinary/3`) loads pre-compiled shader binary code
into the `Count` shader objects whose handles are given in `Shaders`. `Binary`
points to `Length` bytes of binary shader code stored in client memory.
`BinaryFormat` specifies the format of the pre-compiled code.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderBinary.xhtml)

# `shaderSource`

```erlang
-spec shaderSource(Shader :: i(), String :: [unicode:chardata()]) -> ok.
```

[`gl:shaderSource/2`](`shaderSource/2`) sets the source code in `Shader` to the
source code in the array of strings specified by `String`. Any source code
previously stored in the shader object is completely replaced. The number of
strings in the array is specified by `Count`. If `Length` is `?NULL`, each
string is assumed to be null terminated. If `Length` is a value other than
`?NULL`, it points to an array containing a string length for each of the
corresponding elements of `String`. Each element in the `Length` array may
contain the length of the corresponding string (the null character is not
counted as part of the string length) or a value less than 0 to indicate that
the string is null terminated. The source code strings are not scanned or parsed
at this time; they are simply copied into the specified shader object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderSource.xhtml)

# `shaderStorageBlockBinding`

```erlang
-spec shaderStorageBlockBinding(Program :: i(), StorageBlockIndex :: i(), StorageBlockBinding :: i()) ->
                                   ok.
```

[`gl:shaderStorageBlockBinding/3`](`shaderStorageBlockBinding/3`), changes the
active shader storage block with an assigned index of `StorageBlockIndex` in
program object `Program`. `StorageBlockIndex` must be an active shader storage
block index in `Program`. `StorageBlockBinding` must be less than the value of
`?GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS`. If successful,
[`gl:shaderStorageBlockBinding/3`](`shaderStorageBlockBinding/3`) specifies that
`Program` will use the data store of the buffer object bound to the binding
point `StorageBlockBinding` to read and write the values of the buffer variables
in the shader storage block identified by `StorageBlockIndex`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glShaderStorageBlockBinding.xhtml)

# `stencilFunc`

```erlang
-spec stencilFunc(Func :: enum(), Ref :: i(), Mask :: i()) -> ok.
```

Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. Stencil planes are first drawn into using GL drawing primitives, then
geometry and images are rendered using the stencil planes to mask out portions
of the screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFunc.xhtml)

# `stencilFuncSeparate`

```erlang
-spec stencilFuncSeparate(Face :: enum(), Func :: enum(), Ref :: i(), Mask :: i()) -> ok.
```

Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilFuncSeparate.xhtml)

# `stencilMask`

```erlang
-spec stencilMask(Mask :: i()) -> ok.
```

[`gl:stencilMask/1`](`stencilMask/1`) controls the writing of individual bits in
the stencil planes. The least significant n bits of `Mask`, where n is the
number of bits in the stencil buffer, specify a mask. Where a 1 appears in the
mask, it's possible to write to the corresponding bit in the stencil buffer.
Where a 0 appears, the corresponding bit is write-protected. Initially, all bits
are enabled for writing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMask.xhtml)

# `stencilMaskSeparate`

```erlang
-spec stencilMaskSeparate(Face :: enum(), Mask :: i()) -> ok.
```

[`gl:stencilMaskSeparate/2`](`stencilMaskSeparate/2`) controls the writing of
individual bits in the stencil planes. The least significant n bits of `Mask`,
where n is the number of bits in the stencil buffer, specify a mask. Where a 1
appears in the mask, it's possible to write to the corresponding bit in the
stencil buffer. Where a 0 appears, the corresponding bit is write-protected.
Initially, all bits are enabled for writing.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilMaskSeparate.xhtml)

# `stencilOp`

```erlang
-spec stencilOp(Fail :: enum(), Zfail :: enum(), Zpass :: enum()) -> ok.
```

Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOp.xhtml)

# `stencilOpSeparate`

```erlang
-spec stencilOpSeparate(Face :: enum(), Sfail :: enum(), Dpfail :: enum(), Dppass :: enum()) -> ok.
```

Stenciling, like depth-buffering, enables and disables drawing on a per-pixel
basis. You draw into the stencil planes using GL drawing primitives, then render
geometry and images, using the stencil planes to mask out portions of the
screen. Stenciling is typically used in multipass rendering algorithms to
achieve special effects, such as decals, outlining, and constructive solid
geometry rendering.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glStencilOpSeparate.xhtml)

# `texBuffer`

```erlang
-spec texBuffer(Target :: enum(), Internalformat :: enum(), Buffer :: i()) -> ok.
```

# `texBufferRange`

```erlang
-spec texBufferRange(Target :: enum(),
                     Internalformat :: enum(),
                     Buffer :: i(),
                     Offset :: i(),
                     Size :: i()) ->
                        ok.
```

# `texCoord1d`

```erlang
-spec texCoord1d(S :: f()) -> ok.
```

# `texCoord1dv`

```erlang
-spec texCoord1dv({S :: f()}) -> ok.
```

# `texCoord1f`

```erlang
-spec texCoord1f(S :: f()) -> ok.
```

# `texCoord1fv`

```erlang
-spec texCoord1fv({S :: f()}) -> ok.
```

# `texCoord1i`

```erlang
-spec texCoord1i(S :: i()) -> ok.
```

# `texCoord1iv`

```erlang
-spec texCoord1iv({S :: i()}) -> ok.
```

# `texCoord1s`

```erlang
-spec texCoord1s(S :: i()) -> ok.
```

# `texCoord1sv`

```erlang
-spec texCoord1sv({S :: i()}) -> ok.
```

# `texCoord2d`

```erlang
-spec texCoord2d(S :: f(), T :: f()) -> ok.
```

# `texCoord2dv`

```erlang
-spec texCoord2dv({S :: f(), T :: f()}) -> ok.
```

# `texCoord2f`

```erlang
-spec texCoord2f(S :: f(), T :: f()) -> ok.
```

# `texCoord2fv`

```erlang
-spec texCoord2fv({S :: f(), T :: f()}) -> ok.
```

# `texCoord2i`

```erlang
-spec texCoord2i(S :: i(), T :: i()) -> ok.
```

# `texCoord2iv`

```erlang
-spec texCoord2iv({S :: i(), T :: i()}) -> ok.
```

# `texCoord2s`

```erlang
-spec texCoord2s(S :: i(), T :: i()) -> ok.
```

# `texCoord2sv`

```erlang
-spec texCoord2sv({S :: i(), T :: i()}) -> ok.
```

# `texCoord3d`

```erlang
-spec texCoord3d(S :: f(), T :: f(), R :: f()) -> ok.
```

# `texCoord3dv`

```erlang
-spec texCoord3dv({S :: f(), T :: f(), R :: f()}) -> ok.
```

# `texCoord3f`

```erlang
-spec texCoord3f(S :: f(), T :: f(), R :: f()) -> ok.
```

# `texCoord3fv`

```erlang
-spec texCoord3fv({S :: f(), T :: f(), R :: f()}) -> ok.
```

# `texCoord3i`

```erlang
-spec texCoord3i(S :: i(), T :: i(), R :: i()) -> ok.
```

# `texCoord3iv`

```erlang
-spec texCoord3iv({S :: i(), T :: i(), R :: i()}) -> ok.
```

# `texCoord3s`

```erlang
-spec texCoord3s(S :: i(), T :: i(), R :: i()) -> ok.
```

# `texCoord3sv`

```erlang
-spec texCoord3sv({S :: i(), T :: i(), R :: i()}) -> ok.
```

# `texCoord4d`

```erlang
-spec texCoord4d(S :: f(), T :: f(), R :: f(), Q :: f()) -> ok.
```

# `texCoord4dv`

```erlang
-spec texCoord4dv({S :: f(), T :: f(), R :: f(), Q :: f()}) -> ok.
```

# `texCoord4f`

```erlang
-spec texCoord4f(S :: f(), T :: f(), R :: f(), Q :: f()) -> ok.
```

# `texCoord4fv`

```erlang
-spec texCoord4fv({S :: f(), T :: f(), R :: f(), Q :: f()}) -> ok.
```

# `texCoord4i`

```erlang
-spec texCoord4i(S :: i(), T :: i(), R :: i(), Q :: i()) -> ok.
```

# `texCoord4iv`

```erlang
-spec texCoord4iv({S :: i(), T :: i(), R :: i(), Q :: i()}) -> ok.
```

# `texCoord4s`

```erlang
-spec texCoord4s(S :: i(), T :: i(), R :: i(), Q :: i()) -> ok.
```

# `texCoord4sv`

```erlang
-spec texCoord4sv({S :: i(), T :: i(), R :: i(), Q :: i()}) -> ok.
```

[`gl:texCoord()`](`texCoord1d/1`) specifies texture coordinates in one, two,
three, or four dimensions. [`gl:texCoord1()`](`texCoord1d/1`) sets the current
texture coordinates to (s 0 0 1); a call to [`gl:texCoord2()`](`texCoord1d/1`)
sets them to (s t 0 1). Similarly, [`gl:texCoord3()`](`texCoord1d/1`) specifies
the texture coordinates as (s t r 1), and [`gl:texCoord4()`](`texCoord1d/1`)
defines all four components explicitly as (s t r q).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoord.xml)

# `texCoordPointer`

```erlang
-spec texCoordPointer(Size :: i(), Type :: enum(), Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:texCoordPointer/4`](`texCoordPointer/4`) specifies the location and data
format of an array of texture coordinates to use when rendering. `Size`
specifies the number of coordinates per texture coordinate set, and must be 1,
2, 3, or 4. `Type` specifies the data type of each texture coordinate, and
`Stride` specifies the byte stride from one texture coordinate set to the next,
allowing vertices and attributes to be packed into a single array or stored in
separate arrays. (Single-array storage may be more efficient on some
implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexCoordPointer.xml)

# `texEnvf`

```erlang
-spec texEnvf(Target :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `texEnvfv`

```erlang
-spec texEnvfv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texEnvi`

```erlang
-spec texEnvi(Target :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

# `texEnviv`

```erlang
-spec texEnviv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

A texture environment specifies how texture values are interpreted when a
fragment is textured. When `Target` is `?GL_TEXTURE_FILTER_CONTROL`, `Pname`
must be `?GL_TEXTURE_LOD_BIAS`. When `Target` is `?GL_TEXTURE_ENV`, `Pname` can
be `?GL_TEXTURE_ENV_MODE`, `?GL_TEXTURE_ENV_COLOR`, `?GL_COMBINE_RGB`,
`?GL_COMBINE_ALPHA`, `?GL_RGB_SCALE`, `?GL_ALPHA_SCALE`, `?GL_SRC0_RGB`,
`?GL_SRC1_RGB`, `?GL_SRC2_RGB`, `?GL_SRC0_ALPHA`, `?GL_SRC1_ALPHA`, or
`?GL_SRC2_ALPHA`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexEnv.xml)

# `texGend`

```erlang
-spec texGend(Coord :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `texGendv`

```erlang
-spec texGendv(Coord :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texGenf`

```erlang
-spec texGenf(Coord :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `texGenfv`

```erlang
-spec texGenfv(Coord :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texGeni`

```erlang
-spec texGeni(Coord :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

# `texGeniv`

```erlang
-spec texGeniv(Coord :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:texGen()`](`texGend/3`) selects a texture-coordinate generation function or
supplies coefficients for one of the functions. `Coord` names one of the (`s`,
`t`, `r`, `q`) texture coordinates; it must be one of the symbols `?GL_S`,
`?GL_T`, `?GL_R`, or `?GL_Q`. `Pname` must be one of three symbolic constants:
`?GL_TEXTURE_GEN_MODE`, `?GL_OBJECT_PLANE`, or `?GL_EYE_PLANE`. If `Pname` is
`?GL_TEXTURE_GEN_MODE`, then `Params` chooses a mode, one of
`?GL_OBJECT_LINEAR`, `?GL_EYE_LINEAR`, `?GL_SPHERE_MAP`, `?GL_NORMAL_MAP`, or
`?GL_REFLECTION_MAP`. If `Pname` is either `?GL_OBJECT_PLANE` or
`?GL_EYE_PLANE`, `Params` contains coefficients for the corresponding texture
generation function.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTexGen.xml)

# `texImage1D`

```erlang
-spec texImage1D(Target, Level, InternalFormat, Width, Border, Format, Type, Pixels) -> ok
                    when
                        Target :: enum(),
                        Level :: i(),
                        InternalFormat :: i(),
                        Width :: i(),
                        Border :: i(),
                        Format :: enum(),
                        Type :: enum(),
                        Pixels :: offset() | mem().
```

Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable and disable one-dimensional
texturing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_TEXTURE_1D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml)

# `texImage2D`

```erlang
-spec texImage2D(Target, Level, InternalFormat, Width, Height, Border, Format, Type, Pixels) -> ok
                    when
                        Target :: enum(),
                        Level :: i(),
                        InternalFormat :: i(),
                        Width :: i(),
                        Height :: i(),
                        Border :: i(),
                        Format :: enum(),
                        Type :: enum(),
                        Pixels :: offset() | mem().
```

Texturing allows elements of an image array to be read by shaders.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml)

# `texImage2DMultisample`

```erlang
-spec texImage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) -> ok
                               when
                                   Target :: enum(),
                                   Samples :: i(),
                                   Internalformat :: enum(),
                                   Width :: i(),
                                   Height :: i(),
                                   Fixedsamplelocations :: 0 | 1.
```

[`gl:texImage2DMultisample/6`](`texImage2DMultisample/6`) establishes the data
storage, format, dimensions and number of samples of a multisample texture's
image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2DMultisample.xhtml)

# `texImage3D`

```erlang
-spec texImage3D(Target, Level, InternalFormat, Width, Height, Depth, Border, Format, Type, Pixels) ->
                    ok
                    when
                        Target :: enum(),
                        Level :: i(),
                        InternalFormat :: i(),
                        Width :: i(),
                        Height :: i(),
                        Depth :: i(),
                        Border :: i(),
                        Format :: enum(),
                        Type :: enum(),
                        Pixels :: offset() | mem().
```

Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable and disable
three-dimensional texturing, call [`gl:enable/1`](`enable/1`) and
[`gl:disable/1`](`enable/1`) with argument `?GL_TEXTURE_3D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3D.xhtml)

# `texImage3DMultisample`

```erlang
-spec texImage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth, Fixedsamplelocations) ->
                               ok
                               when
                                   Target :: enum(),
                                   Samples :: i(),
                                   Internalformat :: enum(),
                                   Width :: i(),
                                   Height :: i(),
                                   Depth :: i(),
                                   Fixedsamplelocations :: 0 | 1.
```

[`gl:texImage3DMultisample/7`](`texImage3DMultisample/7`) establishes the data
storage, format, dimensions and number of samples of a multisample texture's
image.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage3DMultisample.xhtml)

# `texParameterf`

```erlang
-spec texParameterf(Target :: enum(), Pname :: enum(), Param :: f()) -> ok.
```

# `texParameterfv`

```erlang
-spec texParameterfv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texParameterIiv`

```erlang
-spec texParameterIiv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texParameterIuiv`

```erlang
-spec texParameterIuiv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

# `texParameteri`

```erlang
-spec texParameteri(Target :: enum(), Pname :: enum(), Param :: i()) -> ok.
```

# `texParameteriv`

```erlang
-spec texParameteriv(Target :: enum(), Pname :: enum(), Params :: tuple()) -> ok.
```

[`gl:texParameter()`](`texParameterf/3`) and
[`gl:textureParameter()`](`texParameterf/3`) assign the value or values in
`Params` to the texture parameter specified as `Pname`. For
[`gl:texParameter()`](`texParameterf/3`), `Target` defines the target texture,
either `?GL_TEXTURE_1D`, `?GL_TEXTURE_1D_ARRAY`, `?GL_TEXTURE_2D`,
`?GL_TEXTURE_2D_ARRAY`, `?GL_TEXTURE_2D_MULTISAMPLE`,
`?GL_TEXTURE_2D_MULTISAMPLE_ARRAY`, `?GL_TEXTURE_3D`, `?GL_TEXTURE_CUBE_MAP`,
`?GL_TEXTURE_CUBE_MAP_ARRAY`, or `?GL_TEXTURE_RECTANGLE`. The following symbols
are accepted in `Pname`:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml)

# `texStorage1D`

```erlang
-spec texStorage1D(Target :: enum(), Levels :: i(), Internalformat :: enum(), Width :: i()) -> ok.
```

[`gl:texStorage1D/4`](`texStorage1D/4`) and
[`gl:textureStorage1D()`](`texStorage1D/4`) specify the storage requirements for
all levels of a one-dimensional texture simultaneously. Once a texture is
specified with this command, the format and dimensions of all levels become
immutable unless it is a proxy texture. The contents of the image may still be
modified, however, its storage requirements may not change. Such a texture is
referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage1D.xhtml)

# `texStorage2D`

```erlang
-spec texStorage2D(Target :: enum(),
                   Levels :: i(),
                   Internalformat :: enum(),
                   Width :: i(),
                   Height :: i()) ->
                      ok.
```

[`gl:texStorage2D/5`](`texStorage2D/5`) and
[`gl:textureStorage2D()`](`texStorage2D/5`) specify the storage requirements for
all levels of a two-dimensional texture or one-dimensional texture array
simultaneously. Once a texture is specified with this command, the format and
dimensions of all levels become immutable unless it is a proxy texture. The
contents of the image may still be modified, however, its storage requirements
may not change. Such a texture is referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage2D.xhtml)

# `texStorage2DMultisample`

```erlang
-spec texStorage2DMultisample(Target, Samples, Internalformat, Width, Height, Fixedsamplelocations) ->
                                 ok
                                 when
                                     Target :: enum(),
                                     Samples :: i(),
                                     Internalformat :: enum(),
                                     Width :: i(),
                                     Height :: i(),
                                     Fixedsamplelocations :: 0 | 1.
```

[`gl:texStorage2DMultisample/6`](`texStorage2DMultisample/6`) and
[`gl:textureStorage2DMultisample()`](`texStorage2DMultisample/6`) specify the
storage requirements for a two-dimensional multisample texture. Once a texture
is specified with this command, its format and dimensions become immutable
unless it is a proxy texture. The contents of the image may still be modified,
however, its storage requirements may not change. Such a texture is referred to
as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage2DMultisample.xhtml)

# `texStorage3D`

```erlang
-spec texStorage3D(Target, Levels, Internalformat, Width, Height, Depth) -> ok
                      when
                          Target :: enum(),
                          Levels :: i(),
                          Internalformat :: enum(),
                          Width :: i(),
                          Height :: i(),
                          Depth :: i().
```

[`gl:texStorage3D/6`](`texStorage3D/6`) and
[`gl:textureStorage3D()`](`texStorage3D/6`) specify the storage requirements for
all levels of a three-dimensional, two-dimensional array or cube-map array
texture simultaneously. Once a texture is specified with this command, the
format and dimensions of all levels become immutable unless it is a proxy
texture. The contents of the image may still be modified, however, its storage
requirements may not change. Such a texture is referred to as an
`immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage3D.xhtml)

# `texStorage3DMultisample`

```erlang
-spec texStorage3DMultisample(Target, Samples, Internalformat, Width, Height, Depth,
                              Fixedsamplelocations) ->
                                 ok
                                 when
                                     Target :: enum(),
                                     Samples :: i(),
                                     Internalformat :: enum(),
                                     Width :: i(),
                                     Height :: i(),
                                     Depth :: i(),
                                     Fixedsamplelocations :: 0 | 1.
```

[`gl:texStorage3DMultisample/7`](`texStorage3DMultisample/7`) and
[`gl:textureStorage3DMultisample()`](`texStorage3DMultisample/7`) specify the
storage requirements for a two-dimensional multisample array texture. Once a
texture is specified with this command, its format and dimensions become
immutable unless it is a proxy texture. The contents of the image may still be
modified, however, its storage requirements may not change. Such a texture is
referred to as an `immutable-format` texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexStorage3DMultisample.xhtml)

# `texSubImage1D`

```erlang
-spec texSubImage1D(Target, Level, Xoffset, Width, Format, Type, Pixels) -> ok
                       when
                           Target :: enum(),
                           Level :: i(),
                           Xoffset :: i(),
                           Width :: i(),
                           Format :: enum(),
                           Type :: enum(),
                           Pixels :: offset() | mem().
```

Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled. To enable or disable one-dimensional
texturing, call [`gl:enable/1`](`enable/1`) and [`gl:disable/1`](`enable/1`)
with argument `?GL_TEXTURE_1D`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage1D.xhtml)

# `texSubImage2D`

```erlang
-spec texSubImage2D(Target, Level, Xoffset, Yoffset, Width, Height, Format, Type, Pixels) -> ok
                       when
                           Target :: enum(),
                           Level :: i(),
                           Xoffset :: i(),
                           Yoffset :: i(),
                           Width :: i(),
                           Height :: i(),
                           Format :: enum(),
                           Type :: enum(),
                           Pixels :: offset() | mem().
```

Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage2D.xhtml)

# `texSubImage3D`

```erlang
-spec texSubImage3D(Target, Level, Xoffset, Yoffset, Zoffset, Width, Height, Depth, Format, Type,
                    Pixels) ->
                       ok
                       when
                           Target :: enum(),
                           Level :: i(),
                           Xoffset :: i(),
                           Yoffset :: i(),
                           Zoffset :: i(),
                           Width :: i(),
                           Height :: i(),
                           Depth :: i(),
                           Format :: enum(),
                           Type :: enum(),
                           Pixels :: offset() | mem().
```

Texturing maps a portion of a specified texture image onto each graphical
primitive for which texturing is enabled.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexSubImage3D.xhtml)

# `textureBarrier`

```erlang
-spec textureBarrier() -> ok.
```

The values of rendered fragments are undefined when a shader stage fetches
texels and the same texels are written via fragment shader outputs, even if the
reads and writes are not in the same drawing command. To safely read the result
of a written texel via a texel fetch in a subsequent drawing command, call
[`gl:textureBarrier/0`](`textureBarrier/0`) between the two drawing commands to
guarantee that writes have completed and caches have been invalidated before
subsequent drawing commands are executed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTextureBarrier.xhtml)

# `textureBuffer`

```erlang
-spec textureBuffer(Texture :: i(), Internalformat :: enum(), Buffer :: i()) -> ok.
```

[`gl:texBuffer/3`](`texBuffer/3`) and [`gl:textureBuffer/3`](`texBuffer/3`)
attaches the data store of a specified buffer object to a specified texture
object, and specify the storage format for the texture image found in the buffer
object. The texture object must be a buffer texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexBuffer.xhtml)

# `textureBufferRange`

```erlang
-spec textureBufferRange(Texture :: i(),
                         Internalformat :: enum(),
                         Buffer :: i(),
                         Offset :: i(),
                         Size :: i()) ->
                            ok.
```

[`gl:texBufferRange/5`](`texBufferRange/5`) and
[`gl:textureBufferRange/5`](`texBufferRange/5`) attach a range of the data store
of a specified buffer object to a specified texture object, and specify the
storage format for the texture image found in the buffer object. The texture
object must be a buffer texture.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexBufferRange.xhtml)

# `textureView`

```erlang
-spec textureView(Texture, Target, Origtexture, Internalformat, Minlevel, Numlevels, Minlayer,
                  Numlayers) ->
                     ok
                     when
                         Texture :: i(),
                         Target :: enum(),
                         Origtexture :: i(),
                         Internalformat :: enum(),
                         Minlevel :: i(),
                         Numlevels :: i(),
                         Minlayer :: i(),
                         Numlayers :: i().
```

[`gl:textureView/8`](`textureView/8`) initializes a texture object as an alias,
or view of another texture object, sharing some or all of the parent texture's
data store with the initialized texture. `Texture` specifies a name previously
reserved by a successful call to [`gl:genTextures/1`](`genTextures/1`) but that
has not yet been bound or given a target. `Target` specifies the target for the
newly initialized texture and must be compatible with the target of the parent
texture, given in `Origtexture` as specified in the following table:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTextureView.xhtml)

# `transformFeedbackBufferBase`

```erlang
-spec transformFeedbackBufferBase(Xfb :: i(), Index :: i(), Buffer :: i()) -> ok.
```

[`gl:transformFeedbackBufferBase/3`](`transformFeedbackBufferBase/3`) binds the
buffer object `Buffer` to the binding point at index `Index` of the transform
feedback object `Xfb`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackBufferBase.xhtml)

# `transformFeedbackBufferRange`

```erlang
-spec transformFeedbackBufferRange(Xfb :: i(), Index :: i(), Buffer :: i(), Offset :: i(), Size :: i()) ->
                                      ok.
```

[`gl:transformFeedbackBufferRange/5`](`transformFeedbackBufferRange/5`) binds a
range of the buffer object `Buffer` represented by `Offset` and `Size` to the
binding point at index `Index` of the transform feedback object `Xfb`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackBufferRange.xhtml)

# `transformFeedbackVaryings`

```erlang
-spec transformFeedbackVaryings(Program :: i(), Varyings :: [unicode:chardata()], BufferMode :: enum()) ->
                                   ok.
```

The names of the vertex or geometry shader outputs to be recorded in transform
feedback mode are specified using
[`gl:transformFeedbackVaryings/3`](`transformFeedbackVaryings/3`). When a
geometry shader is active, transform feedback records the values of selected
geometry shader output variables from the emitted vertices. Otherwise, the
values of the selected vertex shader outputs are recorded.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTransformFeedbackVaryings.xhtml)

# `translated`

```erlang
-spec translated(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `translatef`

```erlang
-spec translatef(X :: f(), Y :: f(), Z :: f()) -> ok.
```

[`gl:translate()`](`translated/3`) produces a translation by (x y z). The
current matrix (see [`gl:matrixMode/1`](`matrixMode/1`)) is multiplied by this
translation matrix, with the product replacing the current matrix, as if
[`gl:multMatrix()`](`multMatrixd/1`) were called with the following matrix for
its argument:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glTranslate.xml)

# `uniform1d`

```erlang
-spec uniform1d(Location :: i(), X :: f()) -> ok.
```

# `uniform1dv`

```erlang
-spec uniform1dv(Location :: i(), Value :: [f()]) -> ok.
```

# `uniform1f`

```erlang
-spec uniform1f(Location :: i(), V0 :: f()) -> ok.
```

# `uniform1fv`

```erlang
-spec uniform1fv(Location :: i(), Value :: [f()]) -> ok.
```

# `uniform1i`

```erlang
-spec uniform1i(Location :: i(), V0 :: i()) -> ok.
```

# `uniform1iv`

```erlang
-spec uniform1iv(Location :: i(), Value :: [i()]) -> ok.
```

# `uniform1ui`

```erlang
-spec uniform1ui(Location :: i(), V0 :: i()) -> ok.
```

# `uniform1uiv`

```erlang
-spec uniform1uiv(Location :: i(), Value :: [i()]) -> ok.
```

# `uniform2d`

```erlang
-spec uniform2d(Location :: i(), X :: f(), Y :: f()) -> ok.
```

# `uniform2dv`

```erlang
-spec uniform2dv(Location :: i(), Value :: [{f(), f()}]) -> ok.
```

# `uniform2f`

```erlang
-spec uniform2f(Location :: i(), V0 :: f(), V1 :: f()) -> ok.
```

# `uniform2fv`

```erlang
-spec uniform2fv(Location :: i(), Value :: [{f(), f()}]) -> ok.
```

# `uniform2i`

```erlang
-spec uniform2i(Location :: i(), V0 :: i(), V1 :: i()) -> ok.
```

# `uniform2iv`

```erlang
-spec uniform2iv(Location :: i(), Value :: [{i(), i()}]) -> ok.
```

# `uniform2ui`

```erlang
-spec uniform2ui(Location :: i(), V0 :: i(), V1 :: i()) -> ok.
```

# `uniform2uiv`

```erlang
-spec uniform2uiv(Location :: i(), Value :: [{i(), i()}]) -> ok.
```

# `uniform3d`

```erlang
-spec uniform3d(Location :: i(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `uniform3dv`

```erlang
-spec uniform3dv(Location :: i(), Value :: [{f(), f(), f()}]) -> ok.
```

# `uniform3f`

```erlang
-spec uniform3f(Location :: i(), V0 :: f(), V1 :: f(), V2 :: f()) -> ok.
```

# `uniform3fv`

```erlang
-spec uniform3fv(Location :: i(), Value :: [{f(), f(), f()}]) -> ok.
```

# `uniform3i`

```erlang
-spec uniform3i(Location :: i(), V0 :: i(), V1 :: i(), V2 :: i()) -> ok.
```

# `uniform3iv`

```erlang
-spec uniform3iv(Location :: i(), Value :: [{i(), i(), i()}]) -> ok.
```

# `uniform3ui`

```erlang
-spec uniform3ui(Location :: i(), V0 :: i(), V1 :: i(), V2 :: i()) -> ok.
```

# `uniform3uiv`

```erlang
-spec uniform3uiv(Location :: i(), Value :: [{i(), i(), i()}]) -> ok.
```

# `uniform4d`

```erlang
-spec uniform4d(Location :: i(), X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `uniform4dv`

```erlang
-spec uniform4dv(Location :: i(), Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `uniform4f`

```erlang
-spec uniform4f(Location :: i(), V0 :: f(), V1 :: f(), V2 :: f(), V3 :: f()) -> ok.
```

# `uniform4fv`

```erlang
-spec uniform4fv(Location :: i(), Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `uniform4i`

```erlang
-spec uniform4i(Location :: i(), V0 :: i(), V1 :: i(), V2 :: i(), V3 :: i()) -> ok.
```

# `uniform4iv`

```erlang
-spec uniform4iv(Location :: i(), Value :: [{i(), i(), i(), i()}]) -> ok.
```

# `uniform4ui`

```erlang
-spec uniform4ui(Location :: i(), V0 :: i(), V1 :: i(), V2 :: i(), V3 :: i()) -> ok.
```

# `uniform4uiv`

```erlang
-spec uniform4uiv(Location :: i(), Value :: [{i(), i(), i(), i()}]) -> ok.
```

# `uniformBlockBinding`

```erlang
-spec uniformBlockBinding(Program :: i(), UniformBlockIndex :: i(), UniformBlockBinding :: i()) -> ok.
```

Binding points for active uniform blocks are assigned using
[`gl:uniformBlockBinding/3`](`uniformBlockBinding/3`). Each of a program's
active uniform blocks has a corresponding uniform buffer binding point.
`Program` is the name of a program object for which the command
[`gl:linkProgram/1`](`linkProgram/1`) has been issued in the past.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformBlockBinding.xhtml)

# `uniformMatrix2dv`

```erlang
-spec uniformMatrix2dv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `uniformMatrix2fv`

```erlang
-spec uniformMatrix2fv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f()}]) -> ok.
```

# `uniformMatrix2x3dv`

```erlang
-spec uniformMatrix2x3dv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix2x3fv`

```erlang
-spec uniformMatrix2x3fv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix2x4dv`

```erlang
-spec uniformMatrix2x4dv(Location :: i(),
                         Transpose :: 0 | 1,
                         Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix2x4fv`

```erlang
-spec uniformMatrix2x4fv(Location :: i(),
                         Transpose :: 0 | 1,
                         Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix3dv`

```erlang
-spec uniformMatrix3dv(Location :: i(),
                       Transpose :: 0 | 1,
                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                          ok.
```

# `uniformMatrix3fv`

```erlang
-spec uniformMatrix3fv(Location :: i(),
                       Transpose :: 0 | 1,
                       Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                          ok.
```

# `uniformMatrix3x2dv`

```erlang
-spec uniformMatrix3x2dv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix3x2fv`

```erlang
-spec uniformMatrix3x2fv(Location :: i(), Transpose :: 0 | 1, Value :: [{f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix3x4dv`

```erlang
-spec uniformMatrix3x4dv(Location, Transpose, Value) -> ok
                            when
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `uniformMatrix3x4fv`

```erlang
-spec uniformMatrix3x4fv(Location, Transpose, Value) -> ok
                            when
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `uniformMatrix4dv`

```erlang
-spec uniformMatrix4dv(Location, Transpose, Value) -> ok
                          when
                              Location :: i(),
                              Transpose :: 0 | 1,
                              Value ::
                                  [{f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f()}].
```

# `uniformMatrix4fv`

```erlang
-spec uniformMatrix4fv(Location, Transpose, Value) -> ok
                          when
                              Location :: i(),
                              Transpose :: 0 | 1,
                              Value ::
                                  [{f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f(),
                                    f()}].
```

# `uniformMatrix4x2dv`

```erlang
-spec uniformMatrix4x2dv(Location :: i(),
                         Transpose :: 0 | 1,
                         Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix4x2fv`

```erlang
-spec uniformMatrix4x2fv(Location :: i(),
                         Transpose :: 0 | 1,
                         Value :: [{f(), f(), f(), f(), f(), f(), f(), f()}]) ->
                            ok.
```

# `uniformMatrix4x3dv`

```erlang
-spec uniformMatrix4x3dv(Location, Transpose, Value) -> ok
                            when
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

# `uniformMatrix4x3fv`

```erlang
-spec uniformMatrix4x3fv(Location, Transpose, Value) -> ok
                            when
                                Location :: i(),
                                Transpose :: 0 | 1,
                                Value :: [{f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f(), f()}].
```

[`gl:uniform()`](`uniform1f/2`) modifies the value of a uniform variable or a
uniform variable array. The location of the uniform variable to be modified is
specified by `Location`, which should be a value returned by
[`gl:getUniformLocation/2`](`getUniformLocation/2`).
[`gl:uniform()`](`uniform1f/2`) operates on the program object that was made
part of current state by calling [`gl:useProgram/1`](`useProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml)

# `uniformSubroutinesuiv`

```erlang
-spec uniformSubroutinesuiv(Shadertype :: enum(), Indices :: [i()]) -> ok.
```

[`gl:uniformSubroutines()`](`uniformSubroutinesuiv/2`) loads all active
subroutine uniforms for shader stage `Shadertype` of the current program with
subroutine indices from `Indices`, storing `Indices[i]` into the uniform at
location `I`. `Count` must be equal to the value of
`?GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS` for the program currently in use at
shader stage `Shadertype`. Furthermore, all values in `Indices` must be less
than the value of `?GL_ACTIVE_SUBROUTINES` for the shader stage.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniformSubroutines.xhtml)

# `useProgram`

```erlang
-spec useProgram(Program :: i()) -> ok.
```

[`gl:useProgram/1`](`useProgram/1`) installs the program object specified by
`Program` as part of current rendering state. One or more executables are
created in a program object by successfully attaching shader objects to it with
[`gl:attachShader/2`](`attachShader/2`), successfully compiling the shader
objects with [`gl:compileShader/1`](`compileShader/1`), and successfully linking
the program object with [`gl:linkProgram/1`](`linkProgram/1`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgram.xhtml)

# `useProgramStages`

```erlang
-spec useProgramStages(Pipeline :: i(), Stages :: i(), Program :: i()) -> ok.
```

[`gl:useProgramStages/3`](`useProgramStages/3`) binds executables from a program
object associated with a specified set of shader stages to the program pipeline
object given by `Pipeline`. `Pipeline` specifies the program pipeline object to
which to bind the executables. `Stages` contains a logical combination of bits
indicating the shader stages to use within `Program` with the program pipeline
object `Pipeline`. `Stages` must be a logical combination of
`?GL_VERTEX_SHADER_BIT`, `?GL_TESS_CONTROL_SHADER_BIT`,
`?GL_TESS_EVALUATION_SHADER_BIT`, `?GL_GEOMETRY_SHADER_BIT`,
`?GL_FRAGMENT_SHADER_BIT` and `?GL_COMPUTE_SHADER_BIT`. Additionally, the
special value `?GL_ALL_SHADER_BITS` may be specified to indicate that all
executables contained in `Program` should be installed in `Pipeline`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUseProgramStages.xhtml)

# `validateProgram`

```erlang
-spec validateProgram(Program :: i()) -> ok.
```

[`gl:validateProgram/1`](`validateProgram/1`) checks to see whether the
executables contained in `Program` can execute given the current OpenGL state.
The information generated by the validation process will be stored in
`Program`'s information log. The validation information may consist of an empty
string, or it may be a string containing information about how the current
program object interacts with the rest of current OpenGL state. This provides a
way for OpenGL implementers to convey more information about why the current
program is inefficient, suboptimal, failing to execute, and so on.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgram.xhtml)

# `validateProgramPipeline`

```erlang
-spec validateProgramPipeline(Pipeline :: i()) -> ok.
```

[`gl:validateProgramPipeline/1`](`validateProgramPipeline/1`) instructs the
implementation to validate the shader executables contained in `Pipeline`
against the current GL state. The implementation may use this as an opportunity
to perform any internal shader modifications that may be required to ensure
correct operation of the installed shaders given the current GL state.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glValidateProgramPipeline.xhtml)

# `vertex2d`

```erlang
-spec vertex2d(X :: f(), Y :: f()) -> ok.
```

# `vertex2dv`

```erlang
-spec vertex2dv({X :: f(), Y :: f()}) -> ok.
```

# `vertex2f`

```erlang
-spec vertex2f(X :: f(), Y :: f()) -> ok.
```

# `vertex2fv`

```erlang
-spec vertex2fv({X :: f(), Y :: f()}) -> ok.
```

# `vertex2i`

```erlang
-spec vertex2i(X :: i(), Y :: i()) -> ok.
```

# `vertex2iv`

```erlang
-spec vertex2iv({X :: i(), Y :: i()}) -> ok.
```

# `vertex2s`

```erlang
-spec vertex2s(X :: i(), Y :: i()) -> ok.
```

# `vertex2sv`

```erlang
-spec vertex2sv({X :: i(), Y :: i()}) -> ok.
```

# `vertex3d`

```erlang
-spec vertex3d(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `vertex3dv`

```erlang
-spec vertex3dv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `vertex3f`

```erlang
-spec vertex3f(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `vertex3fv`

```erlang
-spec vertex3fv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `vertex3i`

```erlang
-spec vertex3i(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `vertex3iv`

```erlang
-spec vertex3iv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `vertex3s`

```erlang
-spec vertex3s(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `vertex3sv`

```erlang
-spec vertex3sv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `vertex4d`

```erlang
-spec vertex4d(X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `vertex4dv`

```erlang
-spec vertex4dv({X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `vertex4f`

```erlang
-spec vertex4f(X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `vertex4fv`

```erlang
-spec vertex4fv({X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `vertex4i`

```erlang
-spec vertex4i(X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertex4iv`

```erlang
-spec vertex4iv({X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `vertex4s`

```erlang
-spec vertex4s(X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertex4sv`

```erlang
-spec vertex4sv({X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

[`gl:vertex()`](`vertex2d/2`) commands are used within
[`gl:'begin'/1`](`'begin'/1`)/[`gl:'end'/0`](`'begin'/1`) pairs to specify
point, line, and polygon vertices. The current color, normal, texture
coordinates, and fog coordinate are associated with the vertex when
[`gl:vertex()`](`vertex2d/2`) is called.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertex.xml)

# `vertexArrayAttribBinding`

```erlang
-spec vertexArrayAttribBinding(Vaobj :: i(), Attribindex :: i(), Bindingindex :: i()) -> ok.
```

# `vertexArrayAttribFormat`

```erlang
-spec vertexArrayAttribFormat(Vaobj, Attribindex, Size, Type, Normalized, Relativeoffset) -> ok
                                 when
                                     Vaobj :: i(),
                                     Attribindex :: i(),
                                     Size :: i(),
                                     Type :: enum(),
                                     Normalized :: 0 | 1,
                                     Relativeoffset :: i().
```

# `vertexArrayAttribIFormat`

```erlang
-spec vertexArrayAttribIFormat(Vaobj :: i(),
                               Attribindex :: i(),
                               Size :: i(),
                               Type :: enum(),
                               Relativeoffset :: i()) ->
                                  ok.
```

# `vertexArrayAttribLFormat`

```erlang
-spec vertexArrayAttribLFormat(Vaobj :: i(),
                               Attribindex :: i(),
                               Size :: i(),
                               Type :: enum(),
                               Relativeoffset :: i()) ->
                                  ok.
```

# `vertexArrayBindingDivisor`

```erlang
-spec vertexArrayBindingDivisor(Vaobj :: i(), Bindingindex :: i(), Divisor :: i()) -> ok.
```

# `vertexArrayElementBuffer`

```erlang
-spec vertexArrayElementBuffer(Vaobj :: i(), Buffer :: i()) -> ok.
```

[`gl:vertexArrayElementBuffer/2`](`vertexArrayElementBuffer/2`) binds a buffer
object with id `Buffer` to the element array buffer bind point of a vertex array
object with id `Vaobj`. If `Buffer` is zero, any existing element array buffer
binding to `Vaobj` is removed.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexArrayElementBuffer.xhtml)

# `vertexArrayVertexBuffer`

```erlang
-spec vertexArrayVertexBuffer(Vaobj :: i(),
                              Bindingindex :: i(),
                              Buffer :: i(),
                              Offset :: i(),
                              Stride :: i()) ->
                                 ok.
```

[`gl:bindVertexBuffer/4`](`bindVertexBuffer/4`) and
[`gl:vertexArrayVertexBuffer/5`](`bindVertexBuffer/4`) bind the buffer named
`Buffer` to the vertex buffer binding point whose index is given by
`Bindingindex`. [`gl:bindVertexBuffer/4`](`bindVertexBuffer/4`) modifies the
binding of the currently bound vertex array object, whereas
[`gl:vertexArrayVertexBuffer/5`](`bindVertexBuffer/4`) allows the caller to
specify ID of the vertex array object with an argument named `Vaobj`, for which
the binding should be modified. `Offset` and `Stride` specify the offset of the
first element within the buffer and the distance between elements within the
buffer, respectively, and are both measured in basic machine units.
`Bindingindex` must be less than the value of `?GL_MAX_VERTEX_ATTRIB_BINDINGS`.
`Offset` and `Stride` must be greater than or equal to zero. If `Buffer` is
zero, then any buffer currently bound to the specified binding point is unbound.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexBuffer.xhtml)

# `vertexArrayVertexBuffers`

```erlang
-spec vertexArrayVertexBuffers(Vaobj :: i(),
                               First :: i(),
                               Buffers :: [i()],
                               Offsets :: [i()],
                               Strides :: [i()]) ->
                                  ok.
```

[`gl:bindVertexBuffers/4`](`bindVertexBuffers/4`) and
[`gl:vertexArrayVertexBuffers/5`](`bindVertexBuffers/4`) bind storage from an
array of existing buffer objects to a specified number of consecutive vertex
buffer binding points units in a vertex array object. For
[`gl:bindVertexBuffers/4`](`bindVertexBuffers/4`), the vertex array object is
the currently bound vertex array object. For
[`gl:vertexArrayVertexBuffers/5`](`bindVertexBuffers/4`), `Vaobj` is the name of
the vertex array object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexBuffers.xhtml)

# `vertexAttrib1d`

```erlang
-spec vertexAttrib1d(Index :: i(), X :: f()) -> ok.
```

# `vertexAttrib1dv`

```erlang
-spec vertexAttrib1dv(Index :: i(), {X :: f()}) -> ok.
```

# `vertexAttrib1f`

```erlang
-spec vertexAttrib1f(Index :: i(), X :: f()) -> ok.
```

# `vertexAttrib1fv`

```erlang
-spec vertexAttrib1fv(Index :: i(), {X :: f()}) -> ok.
```

# `vertexAttrib1s`

```erlang
-spec vertexAttrib1s(Index :: i(), X :: i()) -> ok.
```

# `vertexAttrib1sv`

```erlang
-spec vertexAttrib1sv(Index :: i(), {X :: i()}) -> ok.
```

# `vertexAttrib2d`

```erlang
-spec vertexAttrib2d(Index :: i(), X :: f(), Y :: f()) -> ok.
```

# `vertexAttrib2dv`

```erlang
-spec vertexAttrib2dv(Index :: i(), {X :: f(), Y :: f()}) -> ok.
```

# `vertexAttrib2f`

```erlang
-spec vertexAttrib2f(Index :: i(), X :: f(), Y :: f()) -> ok.
```

# `vertexAttrib2fv`

```erlang
-spec vertexAttrib2fv(Index :: i(), {X :: f(), Y :: f()}) -> ok.
```

# `vertexAttrib2s`

```erlang
-spec vertexAttrib2s(Index :: i(), X :: i(), Y :: i()) -> ok.
```

# `vertexAttrib2sv`

```erlang
-spec vertexAttrib2sv(Index :: i(), {X :: i(), Y :: i()}) -> ok.
```

# `vertexAttrib3d`

```erlang
-spec vertexAttrib3d(Index :: i(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `vertexAttrib3dv`

```erlang
-spec vertexAttrib3dv(Index :: i(), {X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `vertexAttrib3f`

```erlang
-spec vertexAttrib3f(Index :: i(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `vertexAttrib3fv`

```erlang
-spec vertexAttrib3fv(Index :: i(), {X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `vertexAttrib3s`

```erlang
-spec vertexAttrib3s(Index :: i(), X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `vertexAttrib3sv`

```erlang
-spec vertexAttrib3sv(Index :: i(), {X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `vertexAttrib4bv`

```erlang
-spec vertexAttrib4bv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4d`

```erlang
-spec vertexAttrib4d(Index :: i(), X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `vertexAttrib4dv`

```erlang
-spec vertexAttrib4dv(Index :: i(), {X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `vertexAttrib4f`

```erlang
-spec vertexAttrib4f(Index :: i(), X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `vertexAttrib4fv`

```erlang
-spec vertexAttrib4fv(Index :: i(), {X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

# `vertexAttrib4iv`

```erlang
-spec vertexAttrib4iv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4Nbv`

```erlang
-spec vertexAttrib4Nbv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4Niv`

```erlang
-spec vertexAttrib4Niv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4Nsv`

```erlang
-spec vertexAttrib4Nsv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4Nub`

```erlang
-spec vertexAttrib4Nub(Index :: i(), X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertexAttrib4Nubv`

```erlang
-spec vertexAttrib4Nubv(Index :: i(), {X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `vertexAttrib4Nuiv`

```erlang
-spec vertexAttrib4Nuiv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4Nusv`

```erlang
-spec vertexAttrib4Nusv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4s`

```erlang
-spec vertexAttrib4s(Index :: i(), X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertexAttrib4sv`

```erlang
-spec vertexAttrib4sv(Index :: i(), {X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `vertexAttrib4ubv`

```erlang
-spec vertexAttrib4ubv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4uiv`

```erlang
-spec vertexAttrib4uiv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttrib4usv`

```erlang
-spec vertexAttrib4usv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttribBinding`

```erlang
-spec vertexAttribBinding(Attribindex :: i(), Bindingindex :: i()) -> ok.
```

[`gl:vertexAttribBinding/2`](`vertexAttribBinding/2`) and
[`gl:vertexArrayAttribBinding/3`](`vertexAttribBinding/2`) establishes an
association between the generic vertex attribute of a vertex array object whose
index is given by `Attribindex`, and a vertex buffer binding whose index is
given by `Bindingindex`. For
[`gl:vertexAttribBinding/2`](`vertexAttribBinding/2`), the vertex array object
affected is that currently bound. For
[`gl:vertexArrayAttribBinding/3`](`vertexAttribBinding/2`), `Vaobj` is the name
of the vertex array object.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribBinding.xhtml)

# `vertexAttribDivisor`

```erlang
-spec vertexAttribDivisor(Index :: i(), Divisor :: i()) -> ok.
```

[`gl:vertexAttribDivisor/2`](`vertexAttribDivisor/2`) modifies the rate at which
generic vertex attributes advance when rendering multiple instances of
primitives in a single draw call. If `Divisor` is zero, the attribute at slot
`Index` advances once per vertex. If `Divisor` is non-zero, the attribute
advances once per `Divisor` instances of the set(s) of vertices being rendered.
An attribute is referred to as instanced if its
`?GL_VERTEX_ATTRIB_ARRAY_DIVISOR` value is non-zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribDivisor.xhtml)

# `vertexAttribFormat`

```erlang
-spec vertexAttribFormat(Attribindex :: i(),
                         Size :: i(),
                         Type :: enum(),
                         Normalized :: 0 | 1,
                         Relativeoffset :: i()) ->
                            ok.
```

# `vertexAttribI1i`

```erlang
-spec vertexAttribI1i(Index :: i(), X :: i()) -> ok.
```

# `vertexAttribI1iv`

```erlang
-spec vertexAttribI1iv(Index :: i(), {X :: i()}) -> ok.
```

# `vertexAttribI1ui`

```erlang
-spec vertexAttribI1ui(Index :: i(), X :: i()) -> ok.
```

# `vertexAttribI1uiv`

```erlang
-spec vertexAttribI1uiv(Index :: i(), {X :: i()}) -> ok.
```

# `vertexAttribI2i`

```erlang
-spec vertexAttribI2i(Index :: i(), X :: i(), Y :: i()) -> ok.
```

# `vertexAttribI2iv`

```erlang
-spec vertexAttribI2iv(Index :: i(), {X :: i(), Y :: i()}) -> ok.
```

# `vertexAttribI2ui`

```erlang
-spec vertexAttribI2ui(Index :: i(), X :: i(), Y :: i()) -> ok.
```

# `vertexAttribI2uiv`

```erlang
-spec vertexAttribI2uiv(Index :: i(), {X :: i(), Y :: i()}) -> ok.
```

# `vertexAttribI3i`

```erlang
-spec vertexAttribI3i(Index :: i(), X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `vertexAttribI3iv`

```erlang
-spec vertexAttribI3iv(Index :: i(), {X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `vertexAttribI3ui`

```erlang
-spec vertexAttribI3ui(Index :: i(), X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `vertexAttribI3uiv`

```erlang
-spec vertexAttribI3uiv(Index :: i(), {X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `vertexAttribI4bv`

```erlang
-spec vertexAttribI4bv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttribI4i`

```erlang
-spec vertexAttribI4i(Index :: i(), X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertexAttribI4iv`

```erlang
-spec vertexAttribI4iv(Index :: i(), {X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `vertexAttribI4sv`

```erlang
-spec vertexAttribI4sv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttribI4ubv`

```erlang
-spec vertexAttribI4ubv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttribI4ui`

```erlang
-spec vertexAttribI4ui(Index :: i(), X :: i(), Y :: i(), Z :: i(), W :: i()) -> ok.
```

# `vertexAttribI4uiv`

```erlang
-spec vertexAttribI4uiv(Index :: i(), {X :: i(), Y :: i(), Z :: i(), W :: i()}) -> ok.
```

# `vertexAttribI4usv`

```erlang
-spec vertexAttribI4usv(Index :: i(), V :: {i(), i(), i(), i()}) -> ok.
```

# `vertexAttribIFormat`

```erlang
-spec vertexAttribIFormat(Attribindex :: i(), Size :: i(), Type :: enum(), Relativeoffset :: i()) -> ok.
```

# `vertexAttribIPointer`

```erlang
-spec vertexAttribIPointer(Index :: i(),
                           Size :: i(),
                           Type :: enum(),
                           Stride :: i(),
                           Pointer :: offset() | mem()) ->
                              ok.
```

# `vertexAttribL1d`

```erlang
-spec vertexAttribL1d(Index :: i(), X :: f()) -> ok.
```

# `vertexAttribL1dv`

```erlang
-spec vertexAttribL1dv(Index :: i(), {X :: f()}) -> ok.
```

# `vertexAttribL2d`

```erlang
-spec vertexAttribL2d(Index :: i(), X :: f(), Y :: f()) -> ok.
```

# `vertexAttribL2dv`

```erlang
-spec vertexAttribL2dv(Index :: i(), {X :: f(), Y :: f()}) -> ok.
```

# `vertexAttribL3d`

```erlang
-spec vertexAttribL3d(Index :: i(), X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `vertexAttribL3dv`

```erlang
-spec vertexAttribL3dv(Index :: i(), {X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `vertexAttribL4d`

```erlang
-spec vertexAttribL4d(Index :: i(), X :: f(), Y :: f(), Z :: f(), W :: f()) -> ok.
```

# `vertexAttribL4dv`

```erlang
-spec vertexAttribL4dv(Index :: i(), {X :: f(), Y :: f(), Z :: f(), W :: f()}) -> ok.
```

The [`gl:vertexAttrib()`](`vertexAttrib1d/2`) family of entry points allows an
application to pass generic vertex attributes in numbered locations.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttrib.xhtml)

# `vertexAttribLFormat`

```erlang
-spec vertexAttribLFormat(Attribindex :: i(), Size :: i(), Type :: enum(), Relativeoffset :: i()) -> ok.
```

# `vertexAttribLPointer`

```erlang
-spec vertexAttribLPointer(Index :: i(),
                           Size :: i(),
                           Type :: enum(),
                           Stride :: i(),
                           Pointer :: offset() | mem()) ->
                              ok.
```

[`gl:vertexAttribFormat/5`](`vertexAttribFormat/5`),
[`gl:vertexAttribIFormat/4`](`vertexAttribIPointer/5`) and
[`gl:vertexAttribLFormat/4`](`vertexAttribIPointer/5`), as well as
[`gl:vertexArrayAttribFormat/6`](`vertexAttribIPointer/5`),
[`gl:vertexArrayAttribIFormat/5`](`vertexAttribIPointer/5`) and
[`gl:vertexArrayAttribLFormat/5`](`vertexAttribIPointer/5`) specify the
organization of data in vertex arrays. The first three calls operate on the
bound vertex array object, whereas the last three ones modify the state of a
vertex array object with ID `Vaobj`. `Attribindex` specifies the index of the
generic vertex attribute array whose data layout is being described, and must be
less than the value of `?GL_MAX_VERTEX_ATTRIBS`.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribFormat.xhtml)

# `vertexAttribPointer`

```erlang
-spec vertexAttribPointer(Index, Size, Type, Normalized, Stride, Pointer) -> ok
                             when
                                 Index :: i(),
                                 Size :: i(),
                                 Type :: enum(),
                                 Normalized :: 0 | 1,
                                 Stride :: i(),
                                 Pointer :: offset() | mem().
```

[`gl:vertexAttribPointer/6`](`vertexAttribPointer/6`),
[`gl:vertexAttribIPointer/5`](`vertexAttribIPointer/5`) and
[`gl:vertexAttribLPointer/5`](`vertexAttribIPointer/5`) specify the location and
data format of the array of generic vertex attributes at index `Index` to use
when rendering. `Size` specifies the number of components per attribute and must
be 1, 2, 3, 4, or `?GL_BGRA`. `Type` specifies the data type of each component,
and `Stride` specifies the byte stride from one attribute to the next, allowing
vertices and attributes to be packed into a single array or stored in separate
arrays.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribPointer.xhtml)

# `vertexBindingDivisor`

```erlang
-spec vertexBindingDivisor(Bindingindex :: i(), Divisor :: i()) -> ok.
```

[`gl:vertexBindingDivisor/2`](`vertexBindingDivisor/2`) and
[`gl:vertexArrayBindingDivisor/3`](`vertexBindingDivisor/2`) modify the rate at
which generic vertex attributes advance when rendering multiple instances of
primitives in a single draw command. If `Divisor` is zero, the attributes using
the buffer bound to `Bindingindex` advance once per vertex. If `Divisor` is
non-zero, the attributes advance once per `Divisor` instances of the set(s) of
vertices being rendered. An attribute is referred to as `instanced` if the
corresponding `Divisor` value is non-zero.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexBindingDivisor.xhtml)

# `vertexPointer`

```erlang
-spec vertexPointer(Size :: i(), Type :: enum(), Stride :: i(), Ptr :: offset() | mem()) -> ok.
```

[`gl:vertexPointer/4`](`vertexPointer/4`) specifies the location and data format
of an array of vertex coordinates to use when rendering. `Size` specifies the
number of coordinates per vertex, and must be 2, 3, or 4. `Type` specifies the
data type of each coordinate, and `Stride` specifies the byte stride from one
vertex to the next, allowing vertices and attributes to be packed into a single
array or stored in separate arrays. (Single-array storage may be more efficient
on some implementations; see [`gl:interleavedArrays/3`](`interleavedArrays/3`).)

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glVertexPointer.xml)

# `viewport`

```erlang
-spec viewport(X :: i(), Y :: i(), Width :: i(), Height :: i()) -> ok.
```

[`gl:viewport/4`](`viewport/4`) specifies the affine transformation of x and y
from normalized device coordinates to window coordinates. Let (x nd y nd) be
normalized device coordinates. Then the window coordinates (x w y w) are
computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewport.xhtml)

# `viewportArrayv`

```erlang
-spec viewportArrayv(First :: i(), V :: [{f(), f(), f(), f()}]) -> ok.
```

[`gl:viewportArrayv/2`](`viewportArrayv/2`) specifies the parameters for
multiple viewports simulataneously. `First` specifies the index of the first
viewport to modify and `Count` specifies the number of viewports to modify.
`First` must be less than the value of `?GL_MAX_VIEWPORTS`, and `First` \+
`Count` must be less than or equal to the value of `?GL_MAX_VIEWPORTS`.
Viewports whose indices lie outside the range [`First`, `First` \+ `Count`) are
not modified. `V` contains the address of an array of floating point values
specifying the left ( x), bottom ( y), width ( w), and height ( h) of each
viewport, in that order. x and y give the location of the viewport's lower left
corner, and w and h give the width and height of the viewport, respectively. The
viewport specifies the affine transformation of x and y from normalized device
coordinates to window coordinates. Let (x nd y nd) be normalized device
coordinates. Then the window coordinates (x w y w) are computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewportArray.xhtml)

# `viewportIndexedf`

```erlang
-spec viewportIndexedf(Index :: i(), X :: f(), Y :: f(), W :: f(), H :: f()) -> ok.
```

# `viewportIndexedfv`

```erlang
-spec viewportIndexedfv(Index :: i(), V :: {f(), f(), f(), f()}) -> ok.
```

[`gl:viewportIndexedf/5`](`viewportIndexedf/5`) and
[`gl:viewportIndexedfv/2`](`viewportIndexedf/5`) specify the parameters for a
single viewport. `Index` specifies the index of the viewport to modify. `Index`
must be less than the value of `?GL_MAX_VIEWPORTS`. For
[`gl:viewportIndexedf/5`](`viewportIndexedf/5`), `X`, `Y`, `W`, and `H` specify
the left, bottom, width and height of the viewport in pixels, respectively. For
[`gl:viewportIndexedfv/2`](`viewportIndexedf/5`), `V` contains the address of an
array of floating point values specifying the left ( x), bottom ( y), width (
w), and height ( h) of each viewport, in that order. x and y give the location
of the viewport's lower left corner, and w and h give the width and height of
the viewport, respectively. The viewport specifies the affine transformation of
x and y from normalized device coordinates to window coordinates. Let (x nd y
nd) be normalized device coordinates. Then the window coordinates (x w y w) are
computed as follows:

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewportIndexed.xhtml)

# `waitSync`

```erlang
-spec waitSync(Sync :: i(), Flags :: i(), Timeout :: i()) -> ok.
```

[`gl:waitSync/3`](`waitSync/3`) causes the GL server to block and wait until
`Sync` becomes signaled. `Sync` is the name of an existing sync object upon
which to wait. `Flags` and `Timeout` are currently not used and must be set to
zero and the special value `?GL_TIMEOUT_IGNORED`, respectively

`Flags` and `Timeout` are placeholders for anticipated future extensions of sync
object capabilities. They must have these reserved values in order that existing
code calling [`gl:waitSync/3`](`waitSync/3`) operate properly in the presence of
such extensions.

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glWaitSync.xhtml)

# `windowPos2d`

```erlang
-spec windowPos2d(X :: f(), Y :: f()) -> ok.
```

# `windowPos2dv`

```erlang
-spec windowPos2dv({X :: f(), Y :: f()}) -> ok.
```

# `windowPos2f`

```erlang
-spec windowPos2f(X :: f(), Y :: f()) -> ok.
```

# `windowPos2fv`

```erlang
-spec windowPos2fv({X :: f(), Y :: f()}) -> ok.
```

# `windowPos2i`

```erlang
-spec windowPos2i(X :: i(), Y :: i()) -> ok.
```

# `windowPos2iv`

```erlang
-spec windowPos2iv({X :: i(), Y :: i()}) -> ok.
```

# `windowPos2s`

```erlang
-spec windowPos2s(X :: i(), Y :: i()) -> ok.
```

# `windowPos2sv`

```erlang
-spec windowPos2sv({X :: i(), Y :: i()}) -> ok.
```

# `windowPos3d`

```erlang
-spec windowPos3d(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `windowPos3dv`

```erlang
-spec windowPos3dv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `windowPos3f`

```erlang
-spec windowPos3f(X :: f(), Y :: f(), Z :: f()) -> ok.
```

# `windowPos3fv`

```erlang
-spec windowPos3fv({X :: f(), Y :: f(), Z :: f()}) -> ok.
```

# `windowPos3i`

```erlang
-spec windowPos3i(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `windowPos3iv`

```erlang
-spec windowPos3iv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

# `windowPos3s`

```erlang
-spec windowPos3s(X :: i(), Y :: i(), Z :: i()) -> ok.
```

# `windowPos3sv`

```erlang
-spec windowPos3sv({X :: i(), Y :: i(), Z :: i()}) -> ok.
```

The GL maintains a 3D position in window coordinates. This position, called the
raster position, is used to position pixel and bitmap write operations. It is
maintained with subpixel accuracy. See [`gl:bitmap/7`](`bitmap/7`),
[`gl:drawPixels/5`](`drawPixels/5`), and [`gl:copyPixels/5`](`copyPixels/5`).

[External documentation.](https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glWindowPos.xml)

---

*Consult [api-reference.md](api-reference.md) for complete listing*
