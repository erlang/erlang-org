# `wxDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDC.erl#L58)

A `m:wxDC` is a `"device context"` onto which graphics and text can be drawn.

It is intended to represent different output devices and offers a common abstract API for
drawing on any of them.

wxWidgets offers an alternative drawing API based on the modern drawing backends GDI+,
CoreGraphics, Cairo and Direct2D. See `m:wxGraphicsContext`, `m:wxGraphicsRenderer` and
related classes. There is also a `m:wxGCDC` linking the APIs by offering the `m:wxDC` API
on top of a `m:wxGraphicsContext`.

`m:wxDC` is an abstract base class and cannot be created directly. Use `m:wxPaintDC`, `m:wxClientDC`, `m:wxWindowDC`, `m:wxScreenDC`, `m:wxMemoryDC`
or `wxPrinterDC` (not implemented in wx). Notice that device contexts which are
associated with windows (i.e. `m:wxClientDC`, `m:wxWindowDC` and `m:wxPaintDC`) use the
window font and colours by default (starting with wxWidgets 2.9.0) but the other device
context classes use system-default values so you always must set the appropriate fonts and
colours before using them.

In addition to the versions of the methods documented below, there are also versions
which accept single {X,Y} parameter instead of the two wxCoord ones or {X,Y} and
{Width,Height} instead of the four wxCoord parameters.

Beginning with wxWidgets 2.9.0 the entire `m:wxDC` code has been reorganized. All
platform dependent code (actually all drawing code) has been moved into backend classes
which derive from a common wxDCImpl class. The user-visible classes such as `m:wxClientDC`
and `m:wxPaintDC` merely forward all calls to the backend implementation.

Device and logical units

In the `m:wxDC` context there is a distinction between `logical` units and `device` units.

`Device` units are the units native to the particular device; e.g. for a screen, a device
unit is a `pixel`. For a printer, the device unit is defined by the resolution of the
printer (usually given in `DPI:` dot-per-inch).

All `m:wxDC` functions use instead `logical` units, unless where explicitly stated.
Logical units are arbitrary units mapped to device units using the current mapping mode
(see `setMapMode/2`).

This mechanism allows reusing the same code which prints on e.g. a window on the screen
to print on e.g. a paper.

Support for Transparency / Alpha Channel

In general `m:wxDC` methods don't support alpha transparency and the alpha component of `wx_color()`
is simply ignored and you need to use `m:wxGraphicsContext` for full transparency support.
There are, however, a few exceptions: first, under macOS and GTK+ 3 colours with alpha
channel are supported in all the normal wxDC-derived classes as they use `m:wxGraphicsContext`
internally. Second, under all platforms `wxSVGFileDC` (not implemented in wx) also fully
supports alpha channel. In both of these cases the instances of `m:wxPen` or `m:wxBrush`
that are built from `wx_color()` use the colour's alpha values when stroking or filling.

Support for Transformation Matrix

On some platforms (currently under MSW, GTK+ 3, macOS) `m:wxDC` has support for applying
an arbitrary affine transformation matrix to its coordinate system (since 3.1.1 this
feature is also supported by `m:wxGCDC` in all ports). Call `CanUseTransformMatrix()` (not
implemented in wx) to check if this support is available and then call `SetTransformMatrix()`
(not implemented in wx) if it is. If the transformation matrix is not supported, `SetTransformMatrix()`
(not implemented in wx) always simply returns `false` and doesn't do anything.

This feature is only available when `wxUSE_DC_TRANSFORM_MATRIX` build option is enabled.

See:
* [Overview dc](https://docs.wxwidgets.org/3.2/overview_dc.html#overview_dc)

* `m:wxGraphicsContext`

wxWidgets docs: [wxDC](https://docs.wxwidgets.org/3.2/classwx_d_c.html)

# `wxDC`

```erlang
-type wxDC() :: wx:wx_object().
```

# `blit`

```erlang
-spec blit(This, Dest, Size, Source, Src) -> boolean()
              when
                  This :: wxDC(),
                  Dest :: {X :: integer(), Y :: integer()},
                  Size :: {W :: integer(), H :: integer()},
                  Source :: wxDC(),
                  Src :: {X :: integer(), Y :: integer()}.
```

# `blit`

```erlang
-spec blit(This, Dest, Size, Source, Src, [Option]) -> boolean()
              when
                  This :: wxDC(),
                  Dest :: {X :: integer(), Y :: integer()},
                  Size :: {W :: integer(), H :: integer()},
                  Source :: wxDC(),
                  Src :: {X :: integer(), Y :: integer()},
                  Option ::
                      {rop, wx:wx_enum()} |
                      {useMask, boolean()} |
                      {srcPtMask, {X :: integer(), Y :: integer()}}.
```

Copy from a source DC to this DC.

With this method you can specify the destination coordinates and the size of area to copy
which will be the same for both the source and target DCs. If you need to apply scaling
while copying, use `StretchBlit()` (not implemented in wx).

Notice that source DC coordinates `xsrc` and `ysrc` are interpreted using the current
source DC coordinate system, i.e. the scale, origin position and axis directions are taken
into account when transforming them to physical (pixel) coordinates.

Remark: There is partial support for `blit/6` in `m:wxPostScriptDC`, under X.

See:
* `m:wxMemoryDC`

* `m:wxBitmap`

* `m:wxMask`

# `calcBoundingBox`

```erlang
-spec calcBoundingBox(This, X, Y) -> ok when This :: wxDC(), X :: integer(), Y :: integer().
```

Adds the specified point to the bounding box which can be retrieved with `minX/1`, `maxX/1`
and `minY/1`, `maxY/1` functions.

See: `resetBoundingBox/1`

# `clear`

```erlang
-spec clear(This) -> ok when This :: wxDC().
```

Clears the device context using the current background brush.

Note that `setBackground/2` method must be used to set the brush used by `clear/1`, the brush used for filling the
shapes set by `setBrush/2` is ignored by it.

If no background brush was set, solid white brush is used to clear the device context.

# `crossHair`

```erlang
-spec crossHair(This, Pt) -> ok when This :: wxDC(), Pt :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `destroyClippingRegion`

```erlang
-spec destroyClippingRegion(This) -> ok when This :: wxDC().
```

Destroys the current clipping region so that none of the DC is clipped.

See: `setClippingRegion/3`

# `deviceToLogicalX`

```erlang
-spec deviceToLogicalX(This, X) -> integer() when This :: wxDC(), X :: integer().
```

Convert `device` X coordinate to logical coordinate, using the current mapping mode, user
scale factor, device origin and axis orientation.

# `deviceToLogicalXRel`

```erlang
-spec deviceToLogicalXRel(This, X) -> integer() when This :: wxDC(), X :: integer().
```

Convert `device` X coordinate to relative logical coordinate, using the current mapping
mode and user scale factor but ignoring the axis orientation.

Use this for converting a width, for example.

# `deviceToLogicalY`

```erlang
-spec deviceToLogicalY(This, Y) -> integer() when This :: wxDC(), Y :: integer().
```

Converts `device` Y coordinate to logical coordinate, using the current mapping mode,
user scale factor, device origin and axis orientation.

# `deviceToLogicalYRel`

```erlang
-spec deviceToLogicalYRel(This, Y) -> integer() when This :: wxDC(), Y :: integer().
```

Convert `device` Y coordinate to relative logical coordinate, using the current mapping
mode and user scale factor but ignoring the axis orientation.

Use this for converting a height, for example.

# `drawArc`

```erlang
-spec drawArc(This, PtStart, PtEnd, Centre) -> ok
                 when
                     This :: wxDC(),
                     PtStart :: {X :: integer(), Y :: integer()},
                     PtEnd :: {X :: integer(), Y :: integer()},
                     Centre :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawBitmap`

```erlang
-spec drawBitmap(This, Bmp, Pt) -> ok
                    when
                        This :: wxDC(),
                        Bmp :: wxBitmap:wxBitmap(),
                        Pt :: {X :: integer(), Y :: integer()}.
```

# `drawBitmap`

```erlang
-spec drawBitmap(This, Bmp, Pt, [Option]) -> ok
                    when
                        This :: wxDC(),
                        Bmp :: wxBitmap:wxBitmap(),
                        Pt :: {X :: integer(), Y :: integer()},
                        Option :: {useMask, boolean()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawCheckMark`

```erlang
-spec drawCheckMark(This, Rect) -> ok
                       when
                           This :: wxDC(),
                           Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawCircle`

```erlang
-spec drawCircle(This, Pt, Radius) -> ok
                    when This :: wxDC(), Pt :: {X :: integer(), Y :: integer()}, Radius :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawEllipse`

```erlang
-spec drawEllipse(This, Rect) -> ok
                     when
                         This :: wxDC(),
                         Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawEllipse`

```erlang
-spec drawEllipse(This, Pt, Size) -> ok
                     when
                         This :: wxDC(),
                         Pt :: {X :: integer(), Y :: integer()},
                         Size :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawEllipticArc`

```erlang
-spec drawEllipticArc(This, Pt, Sz, Sa, Ea) -> ok
                         when
                             This :: wxDC(),
                             Pt :: {X :: integer(), Y :: integer()},
                             Sz :: {W :: integer(), H :: integer()},
                             Sa :: number(),
                             Ea :: number().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawIcon`

```erlang
-spec drawIcon(This, Icon, Pt) -> ok
                  when This :: wxDC(), Icon :: wxIcon:wxIcon(), Pt :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawLabel`

```erlang
-spec drawLabel(This, Text, Rect) -> ok
                   when
                       This :: wxDC(),
                       Text :: unicode:chardata(),
                       Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

# `drawLabel`

```erlang
-spec drawLabel(This, Text, Rect, [Option]) -> ok
                   when
                       This :: wxDC(),
                       Text :: unicode:chardata(),
                       Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                       Option :: {alignment, integer()} | {indexAccel, integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawLine`

```erlang
-spec drawLine(This, Pt1, Pt2) -> ok
                  when
                      This :: wxDC(),
                      Pt1 :: {X :: integer(), Y :: integer()},
                      Pt2 :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawLines`

```erlang
-spec drawLines(This, Points) -> ok when This :: wxDC(), Points :: [{X :: integer(), Y :: integer()}].
```

# `drawLines`

```erlang
-spec drawLines(This, Points, [Option]) -> ok
                   when
                       This :: wxDC(),
                       Points :: [{X :: integer(), Y :: integer()}],
                       Option :: {xoffset, integer()} | {yoffset, integer()}.
```

Draws lines using an array of points of size `n` adding the optional offset coordinate.

The current pen is used for drawing the lines.

# `drawPoint`

```erlang
-spec drawPoint(This, Pt) -> ok when This :: wxDC(), Pt :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawPolygon`

```erlang
-spec drawPolygon(This, Points) -> ok when This :: wxDC(), Points :: [{X :: integer(), Y :: integer()}].
```

# `drawPolygon`

```erlang
-spec drawPolygon(This, Points, [Option]) -> ok
                     when
                         This :: wxDC(),
                         Points :: [{X :: integer(), Y :: integer()}],
                         Option ::
                             {xoffset, integer()} | {yoffset, integer()} | {fillStyle, wx:wx_enum()}.
```

Draws a filled polygon using an array of points of size `n`, adding the optional offset
coordinate.

The first and last points are automatically closed.

The last argument specifies the fill rule: `wxODDEVEN_RULE` (the default) or `wxWINDING_RULE`.

The current pen is used for drawing the outline, and the current brush for filling the
shape. Using a transparent brush suppresses filling.

# `drawRectangle`

```erlang
-spec drawRectangle(This, Rect) -> ok
                       when
                           This :: wxDC(),
                           Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawRectangle`

```erlang
-spec drawRectangle(This, Pt, Sz) -> ok
                       when
                           This :: wxDC(),
                           Pt :: {X :: integer(), Y :: integer()},
                           Sz :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawRotatedText`

```erlang
-spec drawRotatedText(This, Text, Point, Angle) -> ok
                         when
                             This :: wxDC(),
                             Text :: unicode:chardata(),
                             Point :: {X :: integer(), Y :: integer()},
                             Angle :: number().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawRoundedRectangle`

```erlang
-spec drawRoundedRectangle(This, Rect, Radius) -> ok
                              when
                                  This :: wxDC(),
                                  Rect ::
                                      {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                                  Radius :: number().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawRoundedRectangle`

```erlang
-spec drawRoundedRectangle(This, Pt, Sz, Radius) -> ok
                              when
                                  This :: wxDC(),
                                  Pt :: {X :: integer(), Y :: integer()},
                                  Sz :: {W :: integer(), H :: integer()},
                                  Radius :: number().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `drawText`

```erlang
-spec drawText(This, Text, Pt) -> ok
                  when
                      This :: wxDC(), Text :: unicode:chardata(), Pt :: {X :: integer(), Y :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `endDoc`

```erlang
-spec endDoc(This) -> ok when This :: wxDC().
```

Ends a document (only relevant when outputting to a printer).

# `endPage`

```erlang
-spec endPage(This) -> ok when This :: wxDC().
```

Ends a document page (only relevant when outputting to a printer).

# `floodFill`

```erlang
-spec floodFill(This, Pt, Col) -> boolean()
                   when This :: wxDC(), Pt :: {X :: integer(), Y :: integer()}, Col :: wx:wx_colour().
```

# `floodFill`

```erlang
-spec floodFill(This, Pt, Col, [Option]) -> boolean()
                   when
                       This :: wxDC(),
                       Pt :: {X :: integer(), Y :: integer()},
                       Col :: wx:wx_colour(),
                       Option :: {style, wx:wx_enum()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `getBackground`

```erlang
-spec getBackground(This) -> wxBrush:wxBrush() when This :: wxDC().
```

Gets the brush used for painting the background.

See: `setBackground/2`

# `getBackgroundMode`

```erlang
-spec getBackgroundMode(This) -> integer() when This :: wxDC().
```

Returns the current background mode: `wxPENSTYLE\_SOLID` or `wxPENSTYLE\_TRANSPARENT`.

See: `setBackgroundMode/2`

# `getBrush`

```erlang
-spec getBrush(This) -> wxBrush:wxBrush() when This :: wxDC().
```

Gets the current brush.

See: `setBrush/2`

# `getCharHeight`

```erlang
-spec getCharHeight(This) -> integer() when This :: wxDC().
```

Gets the character height of the currently set font.

# `getCharWidth`

```erlang
-spec getCharWidth(This) -> integer() when This :: wxDC().
```

Gets the average character width of the currently set font.

# `getClippingBox`

```erlang
-spec getClippingBox(This) -> Result
                        when
                            Result ::
                                {X :: integer(),
                                 Y :: integer(),
                                 Width :: integer(),
                                 Height :: integer()},
                            This :: wxDC().
```

` Gets the rectangle surrounding the current clipping region. If no clipping region is set
this function returns the extent of the device context. @remarks Clipping region is given
in logical coordinates. @param x If non-<span class='literal'>NULL</span>, filled in with
the logical horizontal coordinate of the top left corner of the clipping region if the
function returns true or 0 otherwise. @param y If non-<span class='literal'>NULL</span>,
filled in with the logical vertical coordinate of the top left corner of the clipping
region if the function returns true or 0 otherwise. @param width If non-<span
class='literal'>NULL</span>, filled in with the width of the clipping region if the
function returns true or the device context width otherwise. @param height If non-<span
class='literal'>NULL</span>, filled in with the height of the clipping region if the
function returns true or the device context height otherwise. `

Return: true if there is a clipping region or false if there is no active clipping region
(note that this return value is available only since wxWidgets 3.1.2, this function didn't
return anything in the previous versions).

# `getFont`

```erlang
-spec getFont(This) -> wxFont:wxFont() when This :: wxDC().
```

Gets the current font.

Notice that even although each device context object has some default font after
creation, this method would return a ?wxNullFont initially and only after calling `setFont/2` a valid
font is returned.

# `getLayoutDirection`

```erlang
-spec getLayoutDirection(This) -> wx:wx_enum() when This :: wxDC().
```

Gets the current layout direction of the device context.

On platforms where RTL layout is supported, the return value will either be `wxLayout_LeftToRight`
or `wxLayout_RightToLeft`. If RTL layout is not supported, the return value will be `wxLayout_Default`.

See: `setLayoutDirection/2`

# `getLogicalFunction`

```erlang
-spec getLogicalFunction(This) -> wx:wx_enum() when This :: wxDC().
```

Gets the current logical function.

See: `setLogicalFunction/2`

# `getMapMode`

```erlang
-spec getMapMode(This) -> wx:wx_enum() when This :: wxDC().
```

Gets the current mapping mode for the device context.

See: `setMapMode/2`

# `getMultiLineTextExtent`

```erlang
-spec getMultiLineTextExtent(This, String) -> {W :: integer(), H :: integer()}
                                when This :: wxDC(), String :: unicode:chardata().
```

Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure.

Return: The text extent as a {Width,Height} object.

Note: This function works with both single-line and multi-line strings.

See:
* `m:wxFont`

* `setFont/2`

* `getPartialTextExtents/2`

* `getTextExtent/3`

# `getMultiLineTextExtent`

```erlang
-spec getMultiLineTextExtent(This, String, [Option]) ->
                                {W :: integer(), H :: integer(), HeightLine :: integer()}
                                when
                                    This :: wxDC(),
                                    String :: unicode:chardata(),
                                    Option :: {font, wxFont:wxFont()}.
```

Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure, `heightLine`, if non NULL, is where to store the
height of a single line.

The text extent is set in the given `w` and `h` pointers.

If the optional parameter `font` is specified and valid, then it is used for the text
extent calculation, otherwise the currently selected font is used.

If `string` is empty, its horizontal extent is 0 but, for convenience when using this
function for allocating enough space for a possibly multi-line string, its vertical extent
is the same as the height of an empty line of text. Please note that this behaviour
differs from that of `getTextExtent/3`.

Note: This function works with both single-line and multi-line strings.

See:
* `m:wxFont`

* `setFont/2`

* `getPartialTextExtents/2`

* `getTextExtent/3`

# `getPartialTextExtents`

```erlang
-spec getPartialTextExtents(This, Text) -> Result
                               when
                                   Result :: {Res :: boolean(), Widths :: [integer()]},
                                   This :: wxDC(),
                                   Text :: unicode:chardata().
```

Fills the `widths` array with the widths from the beginning of `text` to the
corresponding character of `text`.

The generic version simply builds a running total of the widths of each character using `getTextExtent/3`,
however if the various platforms have a native API function that is faster or more
accurate than the generic implementation then it should be used instead.

See:
* `getMultiLineTextExtent/3`

* `getTextExtent/3`

# `getPen`

```erlang
-spec getPen(This) -> wxPen:wxPen() when This :: wxDC().
```

Gets the current pen.

See: `setPen/2`

# `getPixel`

```erlang
-spec getPixel(This, Pos) -> Result
                  when
                      Result :: {Res :: boolean(), Colour :: wx:wx_colour4()},
                      This :: wxDC(),
                      Pos :: {X :: integer(), Y :: integer()}.
```

Gets in `colour` the colour at the specified location.

This method isn't available for `m:wxPostScriptDC` or `wxMetafileDC` (not implemented in
wx) nor for any DC in wxOSX port and simply returns false there.

Note: Setting a pixel can be done using `drawPoint/2`.

Note: This method shouldn't be used with `m:wxPaintDC` as accessing the DC while drawing
can result in unexpected results, notably in wxGTK.

# `getPPI`

```erlang
-spec getPPI(This) -> {W :: integer(), H :: integer()} when This :: wxDC().
```

Returns the resolution of the device in pixels per inch.

# `getSize`

```erlang
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxDC().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `getSizeMM`

```erlang
-spec getSizeMM(This) -> {W :: integer(), H :: integer()} when This :: wxDC().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `getTextBackground`

```erlang
-spec getTextBackground(This) -> wx:wx_colour4() when This :: wxDC().
```

Gets the current text background colour.

See: `setTextBackground/2`

# `getTextExtent`

```erlang
-spec getTextExtent(This, String) -> {W :: integer(), H :: integer()}
                       when This :: wxDC(), String :: unicode:chardata().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `getTextExtent`

```erlang
-spec getTextExtent(This, String, [Option]) -> Result
                       when
                           Result ::
                               {W :: integer(),
                                H :: integer(),
                                Descent :: integer(),
                                ExternalLeading :: integer()},
                           This :: wxDC(),
                           String :: unicode:chardata(),
                           Option :: {theFont, wxFont:wxFont()}.
```

Gets the dimensions of the string using the currently selected font.

`string` is the text string to measure, `descent` is the dimension from the baseline of
the font to the bottom of the descender, and `externalLeading` is any extra vertical space
added to the font by the font designer (usually is zero).

The text extent is returned in `w` and `h` pointers or as a {Width,Height} object
depending on which version of this function is used.

If the optional parameter `font` is specified and valid, then it is used for the text
extent calculation. Otherwise the currently selected font is.

If `string` is empty, its extent is 0 in both directions, as expected.

Note: This function only works with single-line strings.

See:
* `m:wxFont`

* `setFont/2`

* `getPartialTextExtents/2`

* `getMultiLineTextExtent/3`

# `getTextForeground`

```erlang
-spec getTextForeground(This) -> wx:wx_colour4() when This :: wxDC().
```

Gets the current text foreground colour.

See: `setTextForeground/2`

# `getUserScale`

```erlang
-spec getUserScale(This) -> {X :: number(), Y :: number()} when This :: wxDC().
```

Gets the current user scale factor.

See: `setUserScale/3`

# `gradientFillConcentric`

```erlang
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour) -> ok
                                when
                                    This :: wxDC(),
                                    Rect ::
                                        {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                                    InitialColour :: wx:wx_colour(),
                                    DestColour :: wx:wx_colour().
```

Fill the area specified by rect with a radial gradient, starting from `initialColour` at
the centre of the circle and fading to `destColour` on the circle outside.

The circle is placed at the centre of `rect`.

Note: Currently this function is very slow, don't use it for real-time drawing.

# `gradientFillConcentric`

```erlang
-spec gradientFillConcentric(This, Rect, InitialColour, DestColour, CircleCenter) -> ok
                                when
                                    This :: wxDC(),
                                    Rect ::
                                        {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                                    InitialColour :: wx:wx_colour(),
                                    DestColour :: wx:wx_colour(),
                                    CircleCenter :: {X :: integer(), Y :: integer()}.
```

Fill the area specified by rect with a radial gradient, starting from `initialColour` at
the centre of the circle and fading to `destColour` on the circle outside.

`circleCenter` are the relative coordinates of centre of the circle in the specified `rect`.

Note: Currently this function is very slow, don't use it for real-time drawing.

# `gradientFillLinear`

```erlang
-spec gradientFillLinear(This, Rect, InitialColour, DestColour) -> ok
                            when
                                This :: wxDC(),
                                Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                                InitialColour :: wx:wx_colour(),
                                DestColour :: wx:wx_colour().
```

# `gradientFillLinear`

```erlang
-spec gradientFillLinear(This, Rect, InitialColour, DestColour, [Option]) -> ok
                            when
                                This :: wxDC(),
                                Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                                InitialColour :: wx:wx_colour(),
                                DestColour :: wx:wx_colour(),
                                Option :: {nDirection, wx:wx_enum()}.
```

Fill the area specified by `rect` with a linear gradient, starting from `initialColour`
and eventually fading to `destColour`.

The `nDirection` specifies the direction of the colour change, default is to use `initialColour`
on the left part of the rectangle and `destColour` on the right one.

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxDC().
```

Returns true if the DC is ok to use.

# `logicalToDeviceX`

```erlang
-spec logicalToDeviceX(This, X) -> integer() when This :: wxDC(), X :: integer().
```

Converts logical X coordinate to device coordinate, using the current mapping mode, user
scale factor, device origin and axis orientation.

# `logicalToDeviceXRel`

```erlang
-spec logicalToDeviceXRel(This, X) -> integer() when This :: wxDC(), X :: integer().
```

Converts logical X coordinate to relative device coordinate, using the current mapping
mode and user scale factor but ignoring the axis orientation.

Use this for converting a width, for example.

# `logicalToDeviceY`

```erlang
-spec logicalToDeviceY(This, Y) -> integer() when This :: wxDC(), Y :: integer().
```

Converts logical Y coordinate to device coordinate, using the current mapping mode, user
scale factor, device origin and axis orientation.

# `logicalToDeviceYRel`

```erlang
-spec logicalToDeviceYRel(This, Y) -> integer() when This :: wxDC(), Y :: integer().
```

Converts logical Y coordinate to relative device coordinate, using the current mapping
mode and user scale factor but ignoring the axis orientation.

Use this for converting a height, for example.

# `maxX`

```erlang
-spec maxX(This) -> integer() when This :: wxDC().
```

Gets the maximum horizontal extent used in drawing commands so far.

# `maxY`

```erlang
-spec maxY(This) -> integer() when This :: wxDC().
```

Gets the maximum vertical extent used in drawing commands so far.

# `minX`

```erlang
-spec minX(This) -> integer() when This :: wxDC().
```

Gets the minimum horizontal extent used in drawing commands so far.

# `minY`

```erlang
-spec minY(This) -> integer() when This :: wxDC().
```

Gets the minimum vertical extent used in drawing commands so far.

# `resetBoundingBox`

```erlang
-spec resetBoundingBox(This) -> ok when This :: wxDC().
```

Resets the bounding box: after a call to this function, the bounding box doesn't contain
anything.

See: `calcBoundingBox/3`

# `setAxisOrientation`

```erlang
-spec setAxisOrientation(This, XLeftRight, YBottomUp) -> ok
                            when This :: wxDC(), XLeftRight :: boolean(), YBottomUp :: boolean().
```

Sets the x and y axis orientation (i.e. the direction from lowest to highest values on
the axis).

The default orientation is x axis from left to right and y axis from top down.

# `setBackground`

```erlang
-spec setBackground(This, Brush) -> ok when This :: wxDC(), Brush :: wxBrush:wxBrush().
```

Sets the current background brush for the DC.

# `setBackgroundMode`

```erlang
-spec setBackgroundMode(This, Mode) -> ok when This :: wxDC(), Mode :: integer().
```

`mode` may be one of `wxPENSTYLE\_SOLID` and `wxPENSTYLE\_TRANSPARENT`.

This setting determines whether text will be drawn with a background colour or not.

# `setBrush`

```erlang
-spec setBrush(This, Brush) -> ok when This :: wxDC(), Brush :: wxBrush:wxBrush().
```

Sets the current brush for the DC.

If the argument is ?wxNullBrush (or another invalid brush; see `wxBrush:isOk/1`), the current brush is
selected out of the device context (leaving `m:wxDC` without any valid brush), allowing
the current brush to be destroyed safely.

See:
* `m:wxBrush`

* `m:wxMemoryDC`

# `setClippingRegion`

```erlang
-spec setClippingRegion(This, Rect) -> ok
                           when
                               This :: wxDC(),
                               Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setClippingRegion`

```erlang
-spec setClippingRegion(This, Pt, Sz) -> ok
                           when
                               This :: wxDC(),
                               Pt :: {X :: integer(), Y :: integer()},
                               Sz :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setDeviceOrigin`

```erlang
-spec setDeviceOrigin(This, X, Y) -> ok when This :: wxDC(), X :: integer(), Y :: integer().
```

Sets the device origin (i.e. the origin in pixels after scaling has been applied).

This function may be useful in Windows printing operations for placing a graphic on a
page.

# `setFont`

```erlang
-spec setFont(This, Font) -> ok when This :: wxDC(), Font :: wxFont:wxFont().
```

Sets the current font for the DC.

If the argument is ?wxNullFont (or another invalid font; see `wxFont:isOk/1`), the current font is
selected out of the device context (leaving `m:wxDC` without any valid font), allowing the
current font to be destroyed safely.

See: `m:wxFont`

# `setLayoutDirection`

```erlang
-spec setLayoutDirection(This, Dir) -> ok when This :: wxDC(), Dir :: wx:wx_enum().
```

Sets the current layout direction for the device context.

See: `getLayoutDirection/1`

# `setLogicalFunction`

```erlang
-spec setLogicalFunction(This, Function) -> ok when This :: wxDC(), Function :: wx:wx_enum().
```

Sets the current logical function for the device context.

Note: This function is not fully supported in all ports, due to the limitations of the
underlying drawing model. Notably, `wxINVERT` which was commonly used for drawing rubber
bands or other moving outlines in the past, is not, and will not, be supported by wxGTK3
and wxMac. The suggested alternative is to draw temporarily objects normally and refresh
the (affected part of the) window to remove them later.

It determines how a `source` pixel (from a pen or brush colour, or source device context
if using `blit/6`) combines with a `destination` pixel in the current device context. Text drawing
is not affected by this function.

See ?wxRasterOperationMode enumeration values for more info.

The default is `wxCOPY`, which simply draws with the current colour. The others combine
the current colour and the background using a logical operation.

# `setMapMode`

```erlang
-spec setMapMode(This, Mode) -> ok when This :: wxDC(), Mode :: wx:wx_enum().
```

The mapping mode of the device context defines the unit of measurement used to convert `logical`
units to `device` units.

Note that in X, text drawing isn't handled consistently with the mapping mode; a font is
always specified in point size. However, setting the user scale (see `setUserScale/3`) scales the text
appropriately. In Windows, scalable TrueType fonts are always used; in X, results depend
on availability of fonts, but usually a reasonable match is found.

The coordinate origin is always at the top left of the screen/printer.

Drawing to a Windows printer device context uses the current mapping mode, but mapping
mode is currently ignored for PostScript output.

# `setPalette`

```erlang
-spec setPalette(This, Palette) -> ok when This :: wxDC(), Palette :: wxPalette:wxPalette().
```

If this is a window DC or memory DC, assigns the given palette to the window or bitmap
associated with the DC.

If the argument is ?wxNullPalette, the current palette is selected out of the device
context, and the original palette restored.

See: `m:wxPalette`

# `setPen`

```erlang
-spec setPen(This, Pen) -> ok when This :: wxDC(), Pen :: wxPen:wxPen().
```

Sets the current pen for the DC.

If the argument is ?wxNullPen (or another invalid pen; see `wxPen:isOk/1`), the current pen is selected
out of the device context (leaving `m:wxDC` without any valid pen), allowing the current
pen to be destroyed safely.

See: `m:wxMemoryDC`

# `setTextBackground`

```erlang
-spec setTextBackground(This, Colour) -> ok when This :: wxDC(), Colour :: wx:wx_colour().
```

Sets the current text background colour for the DC.

# `setTextForeground`

```erlang
-spec setTextForeground(This, Colour) -> ok when This :: wxDC(), Colour :: wx:wx_colour().
```

Sets the current text foreground colour for the DC.

See: `m:wxMemoryDC`

# `setUserScale`

```erlang
-spec setUserScale(This, XScale, YScale) -> ok
                      when This :: wxDC(), XScale :: number(), YScale :: number().
```

Sets the user scaling factor, useful for applications which require 'zooming'.

# `startDoc`

```erlang
-spec startDoc(This, Message) -> boolean() when This :: wxDC(), Message :: unicode:chardata().
```

Starts a document (only relevant when outputting to a printer).

`message` is a message to show while printing.

# `startPage`

```erlang
-spec startPage(This) -> ok when This :: wxDC().
```

Starts a document page (only relevant when outputting to a printer).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
