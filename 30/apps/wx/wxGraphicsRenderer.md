# `wxGraphicsRenderer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsRenderer.erl#L58)

A `m:wxGraphicsRenderer` is the instance corresponding to the rendering engine used.

There may be multiple instances on a system, if there are different rendering engines
present, but there is always only one instance per engine. This instance is pointed back
to by all objects created by it (`m:wxGraphicsContext`, `m:wxGraphicsPath` etc.) and can
be retrieved through their `wxGraphicsObject:getRenderer/1` method. Therefore you can create an additional instance of a
path etc. by calling `wxGraphicsObject:getRenderer/1` and then using the appropriate CreateXXX() function of that renderer.

wxWidgets docs: [wxGraphicsRenderer](https://docs.wxwidgets.org/3.2/classwx_graphics_renderer.html)

# `wxGraphicsRenderer`

```erlang
-type wxGraphicsRenderer() :: wx:wx_object().
```

# `createBrush`

```erlang
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush()
                     when This :: wxGraphicsRenderer(), Brush :: wxBrush:wxBrush().
```

Creates a native brush from a `m:wxBrush`.

# `createContext`

```erlang
-spec createContext(This, WindowDC) -> wxGraphicsContext:wxGraphicsContext()
                       when
                           This :: wxGraphicsRenderer(),
                           WindowDC ::
                               wxWindowDC:wxWindowDC() | wxWindow:wxWindow() | wxMemoryDC:wxMemoryDC().
```

Creates a `m:wxGraphicsContext` from a `m:wxWindowDC`.

# `createFont`

```erlang
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont()
                    when This :: wxGraphicsRenderer(), Font :: wxFont:wxFont().
```

# `createFont`

```erlang
-spec createFont(This, SizeInPixels, Facename) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsRenderer(),
                        SizeInPixels :: number(),
                        Facename :: unicode:chardata();
                (This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsRenderer(),
                        Font :: wxFont:wxFont(),
                        Option :: {col, wx:wx_colour()}.
```

Creates a native graphics font from a `m:wxFont` and a text colour.

# `createFont`

```erlang
-spec createFont(This, SizeInPixels, Facename, [Option]) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsRenderer(),
                        SizeInPixels :: number(),
                        Facename :: unicode:chardata(),
                        Option :: {flags, integer()} | {col, wx:wx_colour()}.
```

Creates a graphics font with the given characteristics.

If possible, the `createFont/4` overload taking `m:wxFont` should be used instead. The main advantage
of this overload is that it can be used without X server connection under Unix when using Cairo.

Since: 2.9.3

# `createLinearGradientBrush`

```erlang
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, Stops) -> wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsRenderer(),
                                       X1 :: number(),
                                       Y1 :: number(),
                                       X2 :: number(),
                                       Y2 :: number(),
                                       Stops :: wxGraphicsGradientStops:wxGraphicsGradientStops().
```

Creates a native brush with a linear gradient.

Stops support is new since wxWidgets 2.9.1, previously only the start and end colours
could be specified.

The ability to apply a transformation matrix to the gradient was added in 3.1.3

# `createMatrix`

```erlang
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when This :: wxGraphicsRenderer().
```

# `createMatrix`

```erlang
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix()
                      when
                          This :: wxGraphicsRenderer(),
                          Option ::
                              {a, number()} |
                              {b, number()} |
                              {c, number()} |
                              {d, number()} |
                              {tx, number()} |
                              {ty, number()}.
```

Creates a native affine transformation matrix from the passed in values.

The defaults result in an identity matrix.

# `createPath`

```erlang
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when This :: wxGraphicsRenderer().
```

Creates a native graphics path which is initially empty.

# `createRadialGradientBrush`

```erlang
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, Stops) ->
                                   wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsRenderer(),
                                       StartX :: number(),
                                       StartY :: number(),
                                       EndX :: number(),
                                       EndY :: number(),
                                       Radius :: number(),
                                       Stops :: wxGraphicsGradientStops:wxGraphicsGradientStops().
```

Creates a native brush with a radial gradient.

Stops support is new since wxWidgets 2.9.1, previously only the start and end colours
could be specified.

The ability to apply a transformation matrix to the gradient was added in 3.1.3

# `getDefaultRenderer`

```erlang
-spec getDefaultRenderer() -> wxGraphicsRenderer().
```

Returns the default renderer on this platform.

On macOS this is the Core Graphics (a.k.a. Quartz 2D) renderer, on MSW the GDIPlus
renderer, and on GTK we currently default to the Cairo renderer.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
