# `wxGraphicsContext`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGraphicsContext.erl#L58)

A `m:wxGraphicsContext` instance is the object that is drawn upon.

It is created by a renderer using `wxGraphicsRenderer:createContext/2`. This can be either directly using a renderer
instance, or indirectly using the static convenience `create/1` functions of `m:wxGraphicsContext`
that always delegate the task to the default renderer.

Remark: For some renderers (like Direct2D or Cairo) processing of drawing operations may
be deferred (Direct2D render target normally builds up a batch of rendering commands but
defers processing of these commands, Cairo operates on a separate surface) so to make
drawing results visible you need to update the content of the context by calling `wxGraphicsContext::Flush()`
(not implemented in wx) or by destroying the context.

See:
* `wxGraphicsRenderer:createContext/2`

* `m:wxGCDC`

* `m:wxDC`

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsContext](https://docs.wxwidgets.org/3.2/classwx_graphics_context.html)

# `wxGraphicsContext`

```erlang
-type wxGraphicsContext() :: wx:wx_object().
```

# `clip`

```erlang
-spec clip(This, Region) -> ok when This :: wxGraphicsContext(), Region :: wxRegion:wxRegion().
```

Sets the clipping region to the intersection of the given region and the previously set
clipping region.

The clipping region is an area to which drawing is restricted.

Remark:

* Clipping region should be given in logical coordinates.

* Calling this function can only make the clipping region smaller, never larger.

* You need to call `resetClip/1` first if you want to set the clipping region exactly to the region specified.

* If resulting clipping region is empty, then all drawing upon the context is clipped out
(all changes made by drawing operations are masked out).

# `clip`

```erlang
-spec clip(This, X, Y, W, H) -> ok
              when
                  This :: wxGraphicsContext(),
                  X :: number(),
                  Y :: number(),
                  W :: number(),
                  H :: number().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `concatTransform`

```erlang
-spec concatTransform(This, Matrix) -> ok
                         when This :: wxGraphicsContext(), Matrix :: wxGraphicsMatrix:wxGraphicsMatrix().
```

Concatenates the passed in transform with the current transform of this context.

# `create`

```erlang
-spec create() -> wxGraphicsContext().
```

Create a lightweight context that can be used only for measuring text.

# `create`

```erlang
-spec create(WindowDC) -> wxGraphicsContext()
                when
                    WindowDC ::
                        wxWindowDC:wxWindowDC() |
                        wxWindow:wxWindow() |
                        wxMemoryDC:wxMemoryDC() |
                        wxImage:wxImage().
```

Creates a `m:wxGraphicsContext` from a `m:wxWindowDC`.

See: `wxGraphicsRenderer:createContext/2`

# `createBrush`

```erlang
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush()
                     when This :: wxGraphicsContext(), Brush :: wxBrush:wxBrush().
```

Creates a native brush from a `m:wxBrush`.

# `createFont`

```erlang
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont()
                    when This :: wxGraphicsContext(), Font :: wxFont:wxFont().
```

# `createFont`

```erlang
-spec createFont(This, SizeInPixels, Facename) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsContext(),
                        SizeInPixels :: number(),
                        Facename :: unicode:chardata();
                (This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsContext(),
                        Font :: wxFont:wxFont(),
                        Option :: {col, wx:wx_colour()}.
```

Creates a native graphics font from a `m:wxFont` and a text colour.

Remark: For Direct2D graphics fonts can be created from TrueType fonts only.

# `createFont`

```erlang
-spec createFont(This, SizeInPixels, Facename, [Option]) -> wxGraphicsFont:wxGraphicsFont()
                    when
                        This :: wxGraphicsContext(),
                        SizeInPixels :: number(),
                        Facename :: unicode:chardata(),
                        Option :: {flags, integer()} | {col, wx:wx_colour()}.
```

Creates a font object with the specified attributes.

The use of overload taking `m:wxFont` is preferred, see `wxGraphicsRenderer:createFont/4` for more details.

Remark: For Direct2D graphics fonts can be created from TrueType fonts only.

Since: 2.9.3

# `createLinearGradientBrush`

```erlang
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, Stops) -> wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsContext(),
                                       X1 :: number(),
                                       Y1 :: number(),
                                       X2 :: number(),
                                       Y2 :: number(),
                                       Stops :: wxGraphicsGradientStops:wxGraphicsGradientStops().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `createLinearGradientBrush`

```erlang
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsContext(),
                                       X1 :: number(),
                                       Y1 :: number(),
                                       X2 :: number(),
                                       Y2 :: number(),
                                       C1 :: wx:wx_colour(),
                                       C2 :: wx:wx_colour().
```

` Creates a native brush with a linear gradient. The brush starts at (@a x1, @a y1) and
ends at (@a x2, @a y2). Either just the start and end gradient colours (@a c1 and @a c2)
or full set of gradient @a stops can be specified. The version taking
wxGraphicsGradientStops is new in wxWidgets 2.9.1. `
The `matrix` parameter was added in wxWidgets 3.1.3

# `createMatrix`

```erlang
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when This :: wxGraphicsContext().
```

# `createMatrix`

```erlang
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix()
                      when
                          This :: wxGraphicsContext(),
                          Option ::
                              {a, number()} |
                              {b, number()} |
                              {c, number()} |
                              {d, number()} |
                              {tx, number()} |
                              {ty, number()}.
```

Creates a native affine transformation matrix from the passed in values.

The default parameters result in an identity matrix.

# `createPath`

```erlang
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when This :: wxGraphicsContext().
```

Creates a native graphics path which is initially empty.

# `createPen`

```erlang
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen()
                   when This :: wxGraphicsContext(), Pen :: wxPen:wxPen().
```

Creates a native pen from a `m:wxPen`.

Prefer to use the overload taking `wxGraphicsPenInfo` (not implemented in wx) unless you
already have a `m:wxPen` as constructing one only to pass it to this method is wasteful.

# `createRadialGradientBrush`

```erlang
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, Stops) ->
                                   wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsContext(),
                                       StartX :: number(),
                                       StartY :: number(),
                                       EndX :: number(),
                                       EndY :: number(),
                                       Radius :: number(),
                                       Stops :: wxGraphicsGradientStops:wxGraphicsGradientStops().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `createRadialGradientBrush`

```erlang
-spec createRadialGradientBrush(This, StartX, StartY, EndX, EndY, Radius, OColor, CColor) ->
                                   wxGraphicsBrush:wxGraphicsBrush()
                                   when
                                       This :: wxGraphicsContext(),
                                       StartX :: number(),
                                       StartY :: number(),
                                       EndX :: number(),
                                       EndY :: number(),
                                       Radius :: number(),
                                       OColor :: wx:wx_colour(),
                                       CColor :: wx:wx_colour().
```

` Creates a native brush with a radial gradient. The brush originates at (@a startX, @a
startY) and ends on a circle around (@a endX, @a endY) with the given @a radius. The
gradient may be specified either by its start and end colours @a oColor and @a cColor or
by a full set of gradient @a stops. The version taking wxGraphicsGradientStops is new in
wxWidgets 2.9.1. `
The ability to apply a transformation matrix to the gradient was added in 3.1.3

# `destroy`

```erlang
-spec destroy(This :: wxGraphicsContext()) -> ok.
```

Destroys the object

# `drawBitmap`

```erlang
-spec drawBitmap(This, Bmp, X, Y, W, H) -> ok
                    when
                        This :: wxGraphicsContext(),
                        Bmp :: wxBitmap:wxBitmap(),
                        X :: number(),
                        Y :: number(),
                        W :: number(),
                        H :: number().
```

Draws the bitmap.

In case of a mono bitmap, this is treated as a mask and the current brushed is used for
filling.

# `drawEllipse`

```erlang
-spec drawEllipse(This, X, Y, W, H) -> ok
                     when
                         This :: wxGraphicsContext(),
                         X :: number(),
                         Y :: number(),
                         W :: number(),
                         H :: number().
```

Draws an ellipse.

# `drawIcon`

```erlang
-spec drawIcon(This, Icon, X, Y, W, H) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Icon :: wxIcon:wxIcon(),
                      X :: number(),
                      Y :: number(),
                      W :: number(),
                      H :: number().
```

Draws the icon.

# `drawLines`

```erlang
-spec drawLines(This, Points) -> ok
                   when This :: wxGraphicsContext(), Points :: [{X :: float(), Y :: float()}].
```

# `drawLines`

```erlang
-spec drawLines(This, Points, [Option]) -> ok
                   when
                       This :: wxGraphicsContext(),
                       Points :: [{X :: float(), Y :: float()}],
                       Option :: {fillStyle, wx:wx_enum()}.
```

Draws a polygon.

# `drawPath`

```erlang
-spec drawPath(This, Path) -> ok
                  when This :: wxGraphicsContext(), Path :: wxGraphicsPath:wxGraphicsPath().
```

# `drawPath`

```erlang
-spec drawPath(This, Path, [Option]) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Path :: wxGraphicsPath:wxGraphicsPath(),
                      Option :: {fillStyle, wx:wx_enum()}.
```

Draws the path by first filling and then stroking.

# `drawRectangle`

```erlang
-spec drawRectangle(This, X, Y, W, H) -> ok
                       when
                           This :: wxGraphicsContext(),
                           X :: number(),
                           Y :: number(),
                           W :: number(),
                           H :: number().
```

Draws a rectangle.

# `drawRoundedRectangle`

```erlang
-spec drawRoundedRectangle(This, X, Y, W, H, Radius) -> ok
                              when
                                  This :: wxGraphicsContext(),
                                  X :: number(),
                                  Y :: number(),
                                  W :: number(),
                                  H :: number(),
                                  Radius :: number().
```

Draws a rounded rectangle.

# `drawText`

```erlang
-spec drawText(This, Str, X, Y) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Str :: unicode:chardata(),
                      X :: number(),
                      Y :: number().
```

Draws text at the defined position.

# `drawText`

```erlang
-spec drawText(This, Str, X, Y, Angle) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Str :: unicode:chardata(),
                      X :: number(),
                      Y :: number(),
                      Angle :: number();
              (This, Str, X, Y, BackgroundBrush) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Str :: unicode:chardata(),
                      X :: number(),
                      Y :: number(),
                      BackgroundBrush :: wxGraphicsBrush:wxGraphicsBrush().
```

Draws text at the defined position.

# `drawText`

```erlang
-spec drawText(This, Str, X, Y, Angle, BackgroundBrush) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Str :: unicode:chardata(),
                      X :: number(),
                      Y :: number(),
                      Angle :: number(),
                      BackgroundBrush :: wxGraphicsBrush:wxGraphicsBrush().
```

Draws text at the defined position.

# `fillPath`

```erlang
-spec fillPath(This, Path) -> ok
                  when This :: wxGraphicsContext(), Path :: wxGraphicsPath:wxGraphicsPath().
```

# `fillPath`

```erlang
-spec fillPath(This, Path, [Option]) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Path :: wxGraphicsPath:wxGraphicsPath(),
                      Option :: {fillStyle, wx:wx_enum()}.
```

Fills the path with the current brush.

# `getPartialTextExtents`

```erlang
-spec getPartialTextExtents(This, Text) -> [number()]
                               when This :: wxGraphicsContext(), Text :: unicode:chardata().
```

Fills the `widths` array with the widths from the beginning of `text` to the
corresponding character of `text`.

# `getTextExtent`

```erlang
-spec getTextExtent(This, Text) -> Result
                       when
                           Result ::
                               {Width :: number(),
                                Height :: number(),
                                Descent :: number(),
                                ExternalLeading :: number()},
                           This :: wxGraphicsContext(),
                           Text :: unicode:chardata().
```

Gets the dimensions of the string using the currently selected font.

# `getTransform`

```erlang
-spec getTransform(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when This :: wxGraphicsContext().
```

Gets the current transformation matrix of this context.

# `resetClip`

```erlang
-spec resetClip(This) -> ok when This :: wxGraphicsContext().
```

Resets the clipping to original shape.

# `rotate`

```erlang
-spec rotate(This, Angle) -> ok when This :: wxGraphicsContext(), Angle :: number().
```

Rotates the current transformation matrix (in radians).

# `scale`

```erlang
-spec scale(This, XScale, YScale) -> ok
               when This :: wxGraphicsContext(), XScale :: number(), YScale :: number().
```

Scales the current transformation matrix.

# `setBrush`

```erlang
-spec setBrush(This, Brush) -> ok
                  when
                      This :: wxGraphicsContext(),
                      Brush :: wxGraphicsBrush:wxGraphicsBrush() | wxBrush:wxBrush().
```

Sets the brush for filling paths.

# `setFont`

```erlang
-spec setFont(This, Font) -> ok
                 when This :: wxGraphicsContext(), Font :: wxGraphicsFont:wxGraphicsFont().
```

Sets the font for drawing text.

# `setFont`

```erlang
-spec setFont(This, Font, Colour) -> ok
                 when This :: wxGraphicsContext(), Font :: wxFont:wxFont(), Colour :: wx:wx_colour().
```

Sets the font for drawing text.

Remark: For Direct2D only TrueType fonts can be used.

# `setPen`

```erlang
-spec setPen(This, Pen) -> ok
                when This :: wxGraphicsContext(), Pen :: wxPen:wxPen() | wxGraphicsPen:wxGraphicsPen().
```

Sets the pen used for stroking.

# `setTransform`

```erlang
-spec setTransform(This, Matrix) -> ok
                      when This :: wxGraphicsContext(), Matrix :: wxGraphicsMatrix:wxGraphicsMatrix().
```

Sets the current transformation matrix of this context.

# `strokeLine`

```erlang
-spec strokeLine(This, X1, Y1, X2, Y2) -> ok
                    when
                        This :: wxGraphicsContext(),
                        X1 :: number(),
                        Y1 :: number(),
                        X2 :: number(),
                        Y2 :: number().
```

Strokes a single line.

# `strokeLines`

```erlang
-spec strokeLines(This, Points) -> ok
                     when This :: wxGraphicsContext(), Points :: [{X :: float(), Y :: float()}].
```

Stroke lines connecting all the points.

Unlike the other overload of this function, this method draws a single polyline and not a
number of disconnected lines.

# `strokePath`

```erlang
-spec strokePath(This, Path) -> ok
                    when This :: wxGraphicsContext(), Path :: wxGraphicsPath:wxGraphicsPath().
```

Strokes along a path with the current pen.

# `translate`

```erlang
-spec translate(This, Dx, Dy) -> ok when This :: wxGraphicsContext(), Dx :: number(), Dy :: number().
```

Translates the current transformation matrix.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
