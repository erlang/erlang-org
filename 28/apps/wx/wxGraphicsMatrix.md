# `wxGraphicsMatrix`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGraphicsMatrix.erl#L58)

A `m:wxGraphicsMatrix` is a native representation of an affine matrix.

The contents are specific and private to the respective renderer. Instances are ref
counted and can therefore be assigned as usual. The only way to get a valid instance is
via `wxGraphicsContext:createMatrix/2` or `wxGraphicsRenderer:createMatrix/2`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsMatrix](https://docs.wxwidgets.org/3.2/classwx_graphics_matrix.html)

# `wxGraphicsMatrix`

```elixir
-type wxGraphicsMatrix() :: wx:wx_object().
```

# `concat`

```elixir
-spec concat(This, T) -> ok when This :: wxGraphicsMatrix(), T :: wxGraphicsMatrix().
```

Concatenates the matrix passed with the current matrix.

The effect of the resulting transformation is to first apply the transformation in `t` to
the coordinates and then apply the transformation in the current matrix to the coordinates.

# `get`

```elixir
-spec get(This) -> Result
             when
                 Result ::
                     {A :: number(),
                      B :: number(),
                      C :: number(),
                      D :: number(),
                      Tx :: number(),
                      Ty :: number()},
                 This :: wxGraphicsMatrix().
```

Returns the component values of the matrix via the argument pointers.

# `invert`

```elixir
-spec invert(This) -> ok when This :: wxGraphicsMatrix().
```

Inverts the matrix.

# `isEqual`

```elixir
-spec isEqual(This, T) -> boolean() when This :: wxGraphicsMatrix(), T :: wxGraphicsMatrix().
```

Returns true if the elements of the transformation matrix are equal.

# `isIdentity`

```elixir
-spec isIdentity(This) -> boolean() when This :: wxGraphicsMatrix().
```

Return true if this is the identity matrix.

# `rotate`

```elixir
-spec rotate(This, Angle) -> ok when This :: wxGraphicsMatrix(), Angle :: number().
```

Rotates this matrix clockwise (in radians).

# `scale`

```elixir
-spec scale(This, XScale, YScale) -> ok
               when This :: wxGraphicsMatrix(), XScale :: number(), YScale :: number().
```

Scales this matrix.

# `set`

```elixir
-spec set(This) -> ok when This :: wxGraphicsMatrix().
```

# `set`

```elixir
-spec set(This, [Option]) -> ok
             when
                 This :: wxGraphicsMatrix(),
                 Option ::
                     {a, number()} |
                     {b, number()} |
                     {c, number()} |
                     {d, number()} |
                     {tx, number()} |
                     {ty, number()}.
```

Sets the matrix to the respective values (default values are the identity matrix).

# `transformDistance`

```elixir
-spec transformDistance(This) -> {Dx :: number(), Dy :: number()} when This :: wxGraphicsMatrix().
```

Applies this matrix to a distance (ie.

performs all transforms except translations).

# `transformPoint`

```elixir
-spec transformPoint(This) -> {X :: number(), Y :: number()} when This :: wxGraphicsMatrix().
```

Applies this matrix to a point.

# `translate`

```elixir
-spec translate(This, Dx, Dy) -> ok when This :: wxGraphicsMatrix(), Dx :: number(), Dy :: number().
```

Translates this matrix.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
