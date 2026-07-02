# `wxGLCanvas`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGLCanvas.erl#L58)

`m:wxGLCanvas` is a class for displaying OpenGL graphics.

It is always used in conjunction with `m:wxGLContext` as the context can only be made
current (i.e. active for the OpenGL commands) when it is associated to a `m:wxGLCanvas`.

More precisely, you first need to create a `m:wxGLCanvas` window and then create an
instance of a `m:wxGLContext` that is initialized with this `m:wxGLCanvas` and then later
use either `setCurrent/2` with the instance of the `m:wxGLContext` or `wxGLContext:setCurrent/2` with the instance of the `m:wxGLCanvas`
(which might be not the same as was used for the creation of the context) to bind the
OpenGL state that is represented by the rendering context to the canvas, and then finally
call `swapBuffers/1` to swap the buffers of the OpenGL canvas and thus show your current output.

Please note that `m:wxGLContext` always uses physical pixels, even on the platforms where `m:wxWindow`
uses logical pixels, affected by the coordinate scaling, on high DPI displays. Thus, if
you want to set the OpenGL view port to the size of entire window, you must multiply the
result returned by `wxWindow:getClientSize/1` by `wxWindow:getContentScaleFactor/1` before passing it to `glViewport()`. Same considerations apply to
other OpenGL functions and other coordinates, notably those retrieved from `m:wxMouseEvent`
in the event handlers.

Notice that versions of wxWidgets previous to 2.9 used to implicitly create a `m:wxGLContext`
inside `m:wxGLCanvas` itself. This is still supported in the current version but is
deprecated now and will be removed in the future, please update your code to create the
rendering contexts explicitly.

To set up the attributes for the canvas (number of bits for the depth buffer, number of
bits for the stencil buffer and so on) you pass them in the constructor using a `wxGLAttributes`
(not implemented in wx) instance. You can still use the way before 3.1.0 (setting up the
correct values of the `attribList` parameter) but it's discouraged.

Note: On those platforms which use a configure script (e.g. Linux and macOS) OpenGL
support is automatically enabled if the relative headers and libraries are found. To
switch it on under the other platforms (e.g. Windows), you need to edit the `setup.h` file
and set `wxUSE_GLCANVAS` to `1` and then also pass `USE_OPENGL=1` to the make utility. You
may also need to add `opengl32.lib` (and `glu32.lib` for old OpenGL versions) to the list
of the libraries your program is linked with.

See: `m:wxGLContext`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxGLCanvas](https://docs.wxwidgets.org/3.2/classwx_g_l_canvas.html)

# `wxGLCanvas`

```elixir
-type wxGLCanvas() :: wx:wx_object().
```

# `createSurface`

```elixir
-spec createSurface(This) -> boolean() when This :: wxGLCanvas().
```

# `destroy`

```elixir
-spec destroy(This :: wxGLCanvas()) -> ok.
```

Destroys the object

# `isDisplaySupported`

```elixir
-spec isDisplaySupported(AttribList) -> boolean() when AttribList :: [integer()].
```

Determines if a canvas having the specified attributes is available.

This only applies for visual attributes, not rendering context attributes. Please, use
the new form of this method, using `wxGLAttributes` (not implemented in wx).

Return: true if attributes are supported.

# `new`

```elixir
-spec new(Parent) -> wxGLCanvas() when Parent :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Parent, [Option]) -> wxGLCanvas()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {attribList, [integer()]} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {name, unicode:chardata()} |
                     {palette, wxPalette:wxPalette()}.
```

This constructor is still available only for compatibility reasons.

Please use the constructor with `wxGLAttributes` (not implemented in wx) instead.

If `attribList` is not specified, `wxGLAttributes::PlatformDefaults()` (not implemented
in wx) is used, plus some other attributes (see below).

# `setCurrent`

```elixir
-spec setCurrent(This, Context) -> boolean()
                    when This :: wxGLCanvas(), Context :: wxGLContext:wxGLContext().
```

Makes the OpenGL state that is represented by the OpenGL rendering context `context`
current, i.e.

it will be used by all subsequent OpenGL calls.

This is equivalent to `wxGLContext:setCurrent/2` called with this window as parameter.

Note: This function may only be called when the window is shown on screen, in particular
it can't usually be called from the constructor as the window isn't yet shown at this moment.

Return: false if an error occurred.

# `swapBuffers`

```elixir
-spec swapBuffers(This) -> boolean() when This :: wxGLCanvas().
```

Swaps the double-buffer of this window, making the back-buffer the front-buffer and vice
versa, so that the output of the previous OpenGL commands is displayed on the window.

Return: false if an error occurred.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
