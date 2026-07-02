# `wxGraphicsPath`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGraphicsPath.erl#L58)

A `m:wxGraphicsPath` is a native representation of a geometric path.

The contents are specific and private to the respective renderer. Instances are reference
counted and can therefore be assigned as usual. The only way to get a valid instance is by
using `wxGraphicsContext:createPath/1` or `wxGraphicsRenderer:createPath/1`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsPath](https://docs.wxwidgets.org/3.2/classwx_graphics_path.html)

# `wxGraphicsPath`

```elixir
-type wxGraphicsPath() :: wx:wx_object().
```

# `addArc`

```elixir
-spec addArc(This, C, R, StartAngle, EndAngle, Clockwise) -> ok
                when
                    This :: wxGraphicsPath(),
                    C :: {X :: float(), Y :: float()},
                    R :: number(),
                    StartAngle :: number(),
                    EndAngle :: number(),
                    Clockwise :: boolean().
```

# `addArc`

```elixir
-spec addArc(This, X, Y, R, StartAngle, EndAngle, Clockwise) -> ok
                when
                    This :: wxGraphicsPath(),
                    X :: number(),
                    Y :: number(),
                    R :: number(),
                    StartAngle :: number(),
                    EndAngle :: number(),
                    Clockwise :: boolean().
```

Adds an arc of a circle.

The circle is defined by the coordinates of its centre (`x`, `y`) or `c` and its radius `r`.
The arc goes from the starting angle `startAngle` to `endAngle` either clockwise or
counter-clockwise depending on the value of `clockwise` argument.

The angles are measured in radians but, contrary to the usual mathematical convention,
are always `clockwise` from the horizontal axis.

If for clockwise arc `endAngle` is less than `startAngle` it will be progressively
increased by 2*pi until it is greater than `startAngle`. If for counter-clockwise arc `endAngle`
is greater than `startAngle` it will be progressively decreased by 2*pi until it is less
than `startAngle`.

If there is a current point set, an initial line segment will be added to the path to
connect the current point to the beginning of the arc.

# `addArcToPoint`

```elixir
-spec addArcToPoint(This, X1, Y1, X2, Y2, R) -> ok
                       when
                           This :: wxGraphicsPath(),
                           X1 :: number(),
                           Y1 :: number(),
                           X2 :: number(),
                           Y2 :: number(),
                           R :: number().
```

Adds an arc (of a circle with radius `r`) that is tangent to the line connecting current
point and (`x1`, `y1`) and to the line connecting (`x1`, `y1`) and (`x2`, `y2`).

If the current point and the starting point of the arc are different, a straight line
connecting these points is also appended. If there is no current point before the call to `addArcToPoint/6`
this function will behave as if preceded by a call to MoveToPoint(0, 0). After this call
the current point will be at the ending point of the arc.

# `addCircle`

```elixir
-spec addCircle(This, X, Y, R) -> ok
                   when This :: wxGraphicsPath(), X :: number(), Y :: number(), R :: number().
```

Appends a circle around (`x`,`y`) with radius `r` as a new closed subpath.

After this call the current point will be at (x+`r`, `y`).

# `addCurveToPoint`

```elixir
-spec addCurveToPoint(This, C1, C2, E) -> ok
                         when
                             This :: wxGraphicsPath(),
                             C1 :: {X :: float(), Y :: float()},
                             C2 :: {X :: float(), Y :: float()},
                             E :: {X :: float(), Y :: float()}.
```

Adds a cubic bezier curve from the current point, using two control points and an end
point.

If there is no current point before the call to `addCurveToPoint/7` this function will behave as if preceded
by a call to MoveToPoint(`c1`).

# `addCurveToPoint`

```elixir
-spec addCurveToPoint(This, Cx1, Cy1, Cx2, Cy2, X, Y) -> ok
                         when
                             This :: wxGraphicsPath(),
                             Cx1 :: number(),
                             Cy1 :: number(),
                             Cx2 :: number(),
                             Cy2 :: number(),
                             X :: number(),
                             Y :: number().
```

Adds a cubic bezier curve from the current point, using two control points and an end
point.

If there is no current point before the call to `addCurveToPoint/7` this function will behave as if preceded
by a call to MoveToPoint(`cx1`, `cy1`).

# `addEllipse`

```elixir
-spec addEllipse(This, X, Y, W, H) -> ok
                    when
                        This :: wxGraphicsPath(),
                        X :: number(),
                        Y :: number(),
                        W :: number(),
                        H :: number().
```

Appends an ellipse fitting into the passed in rectangle as a new closed subpath.

After this call the current point will be at (x+`w`, y+`h/2`).

# `addLineToPoint`

```elixir
-spec addLineToPoint(This, P) -> ok when This :: wxGraphicsPath(), P :: {X :: float(), Y :: float()}.
```

Adds a straight line from the current point to `p`.

If current point is not yet set before the call to `addLineToPoint/3` this function will behave as `moveToPoint/3`.

# `addLineToPoint`

```elixir
-spec addLineToPoint(This, X, Y) -> ok when This :: wxGraphicsPath(), X :: number(), Y :: number().
```

Adds a straight line from the current point to (`x`,`y`).

If current point is not yet set before the call to `addLineToPoint/3` this function will behave as `moveToPoint/3`.

# `addPath`

```elixir
-spec addPath(This, Path) -> ok when This :: wxGraphicsPath(), Path :: wxGraphicsPath().
```

Adds another path onto the current path.

After this call the current point will be at the added path's current point. For Direct2D
the path being appended shouldn't contain a started non-empty subpath when this function
is called.

# `addQuadCurveToPoint`

```elixir
-spec addQuadCurveToPoint(This, Cx, Cy, X, Y) -> ok
                             when
                                 This :: wxGraphicsPath(),
                                 Cx :: number(),
                                 Cy :: number(),
                                 X :: number(),
                                 Y :: number().
```

Adds a quadratic bezier curve from the current point, using a control point and an end
point.

If there is no current point before the call to `addQuadCurveToPoint/5` this function will behave as if preceded
by a call to MoveToPoint(`cx`, `cy`).

# `addRectangle`

```elixir
-spec addRectangle(This, X, Y, W, H) -> ok
                      when
                          This :: wxGraphicsPath(),
                          X :: number(),
                          Y :: number(),
                          W :: number(),
                          H :: number().
```

Appends a rectangle as a new closed subpath.

After this call the current point will be at (`x`, `y`).

# `addRoundedRectangle`

```elixir
-spec addRoundedRectangle(This, X, Y, W, H, Radius) -> ok
                             when
                                 This :: wxGraphicsPath(),
                                 X :: number(),
                                 Y :: number(),
                                 W :: number(),
                                 H :: number(),
                                 Radius :: number().
```

Appends a rounded rectangle as a new closed subpath.

If `radius` equals 0 this function will behave as `addRectangle/5`, otherwise after this call the current
point will be at (x+`w`, y+`h/2`).

# `closeSubpath`

```elixir
-spec closeSubpath(This) -> ok when This :: wxGraphicsPath().
```

Closes the current sub-path.

After this call the current point will be at the joined endpoint of the sub-path.

# `contains`

```elixir
-spec contains(This, C) -> boolean() when This :: wxGraphicsPath(), C :: {X :: float(), Y :: float()}.
```

# `contains`

```elixir
-spec contains(This, X, Y) -> boolean() when This :: wxGraphicsPath(), X :: number(), Y :: number();
              (This, C, [Option]) -> boolean()
                  when
                      This :: wxGraphicsPath(),
                      C :: {X :: float(), Y :: float()},
                      Option :: {fillStyle, wx:wx_enum()}.
```

Return: true if the point is within the path.

# `contains`

```elixir
-spec contains(This, X, Y, [Option]) -> boolean()
                  when
                      This :: wxGraphicsPath(),
                      X :: number(),
                      Y :: number(),
                      Option :: {fillStyle, wx:wx_enum()}.
```

Return: true if the point is within the path.

# `getBox`

```elixir
-spec getBox(This) -> {X :: float(), Y :: float(), W :: float(), H :: float()}
                when This :: wxGraphicsPath().
```

Gets the bounding box enclosing all points (possibly including control points).

# `getCurrentPoint`

```elixir
-spec getCurrentPoint(This) -> {X :: float(), Y :: float()} when This :: wxGraphicsPath().
```

Gets the last point of the current path, (0,0) if not yet set.

# `moveToPoint`

```elixir
-spec moveToPoint(This, P) -> ok when This :: wxGraphicsPath(), P :: {X :: float(), Y :: float()}.
```

Begins a new subpath at `p`.

# `moveToPoint`

```elixir
-spec moveToPoint(This, X, Y) -> ok when This :: wxGraphicsPath(), X :: number(), Y :: number().
```

Begins a new subpath at (`x`,`y`).

# `transform`

```elixir
-spec transform(This, Matrix) -> ok
                   when This :: wxGraphicsPath(), Matrix :: wxGraphicsMatrix:wxGraphicsMatrix().
```

Transforms each point of this path by the matrix.

For Direct2D the current path shouldn't contain a started non-empty subpath when this
function is called.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
