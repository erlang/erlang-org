# `wxRegion`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxRegion.erl#L58)

A `m:wxRegion` represents a simple or complex region on a device context or window.

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

Predefined objects (include wx.hrl):

* ?wxNullRegion

wxWidgets docs: [wxRegion](https://docs.wxwidgets.org/3.2/classwx_region.html)

# `wxRegion`

```elixir
-type wxRegion() :: wx:wx_object().
```

# `clear`

```elixir
-spec clear(This) -> ok when This :: wxRegion().
```

Clears the current region.

The object becomes invalid, or null, after being cleared.

# `contains`

```elixir
-spec contains(This, Pt) -> wx:wx_enum() when This :: wxRegion(), Pt :: {X :: integer(), Y :: integer()};
              (This, Rect) -> wx:wx_enum()
                  when
                      This :: wxRegion(),
                      Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

Returns a value indicating whether the given rectangle is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: One of ?wxOutRegion, ?wxPartRegion or ?wxInRegion.

Note: On Windows, only ?wxOutRegion and ?wxInRegion are returned; a value ?wxInRegion
then indicates that all or some part of the region is contained in this region.

# `contains`

```elixir
-spec contains(This, X, Y) -> wx:wx_enum() when This :: wxRegion(), X :: integer(), Y :: integer().
```

Returns a value indicating whether the given point is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: The return value is one of `wxOutRegion` and `wxInRegion`.

# `contains`

```elixir
-spec contains(This, X, Y, Width, Height) -> wx:wx_enum()
                  when
                      This :: wxRegion(),
                      X :: integer(),
                      Y :: integer(),
                      Width :: integer(),
                      Height :: integer().
```

Returns a value indicating whether the given rectangle is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: One of ?wxOutRegion, ?wxPartRegion or ?wxInRegion.

Note: On Windows, only ?wxOutRegion and ?wxInRegion are returned; a value ?wxInRegion
then indicates that all or some part of the region is contained in this region.

# `convertToBitmap`

```elixir
-spec convertToBitmap(This) -> wxBitmap:wxBitmap() when This :: wxRegion().
```

Convert the region to a black and white bitmap with the white pixels being inside the
region.

This method can't be used for invalid region.

# `destroy`

```elixir
-spec destroy(This :: wxRegion()) -> ok.
```

Destroys the object

# `getBox`

```elixir
-spec getBox(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                when This :: wxRegion().
```

# `intersect`

```elixir
-spec intersect(This, Rect) -> boolean()
                   when
                       This :: wxRegion(),
                       Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()};
               (This, Region) -> boolean() when This :: wxRegion(), Region :: wxRegion().
```

Finds the intersection of this region and another region.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: Creates the intersection of the two regions, that is, the parts which are in both
regions. The result is stored in this region.

# `intersect`

```elixir
-spec intersect(This, X, Y, Width, Height) -> boolean()
                   when
                       This :: wxRegion(),
                       X :: integer(),
                       Y :: integer(),
                       Width :: integer(),
                       Height :: integer().
```

Finds the intersection of this region and another, rectangular region, specified using
position and size.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: Creates the intersection of the two regions, that is, the parts which are in both
regions. The result is stored in this region.

# `isEmpty`

```elixir
-spec isEmpty(This) -> boolean() when This :: wxRegion().
```

Returns true if the region is empty, false otherwise.

Always returns true if the region is invalid.

# `new`

```elixir
-spec new() -> wxRegion().
```

Default constructor.

This constructor creates an invalid, or null, object, i.e. calling IsOk() on it returns
false and `isEmpty/1` returns true.

# `new`

```elixir
-spec new(Rect) -> wxRegion()
             when Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()};
         (Bmp) -> wxRegion() when Bmp :: wxBitmap:wxBitmap().
```

Constructs a region using a bitmap.

See `union/5` for more details.

# `new`

```elixir
-spec new(TopLeft, BottomRight) -> wxRegion()
             when
                 TopLeft :: {X :: integer(), Y :: integer()},
                 BottomRight :: {X :: integer(), Y :: integer()}.
```

Constructs a rectangular region from the top left point and the bottom right point.

# `new`

```elixir
-spec new(X, Y, Width, Height) -> wxRegion()
             when X :: integer(), Y :: integer(), Width :: integer(), Height :: integer().
```

Constructs a rectangular region with the given position and size.

# `offset`

```elixir
-spec offset(This, Pt) -> boolean() when This :: wxRegion(), Pt :: {X :: integer(), Y :: integer()}.
```

# `offset`

```elixir
-spec offset(This, X, Y) -> boolean() when This :: wxRegion(), X :: integer(), Y :: integer().
```

Moves the region by the specified offsets in horizontal and vertical directions.

This method can't be called if the region is invalid as it doesn't make sense to offset
it then. Attempts to do it will result in assert failure.

Return: true if successful, false otherwise (the region is unchanged then).

# `subtract`

```elixir
-spec subtract(This, Rect) -> boolean()
                  when
                      This :: wxRegion(),
                      Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()};
              (This, Region) -> boolean() when This :: wxRegion(), Region :: wxRegion().
```

Subtracts a region from this region.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: This operation combines the parts of 'this' region that are not part of the
second region. The result is stored in this region.

# `union`

```elixir
-spec union(This, Region) -> boolean()
               when This :: wxRegion(), Region :: wxRegion:wxRegion() | wxBitmap:wxBitmap();
           (This, Rect) -> boolean()
               when
                   This :: wxRegion(),
                   Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

Finds the union of this region and another, rectangular region.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.

# `union`

```elixir
-spec union(This, Bmp, TransColour) -> boolean()
               when This :: wxRegion(), Bmp :: wxBitmap:wxBitmap(), TransColour :: wx:wx_colour().
```

# `union`

```elixir
-spec union(This, Bmp, TransColour, [Option]) -> boolean()
               when
                   This :: wxRegion(),
                   Bmp :: wxBitmap:wxBitmap(),
                   TransColour :: wx:wx_colour(),
                   Option :: {tolerance, integer()}.
```

Finds the union of this region and the non-transparent pixels of a bitmap.

Colour to be treated as transparent is specified in the `transColour` argument, along
with an optional colour tolerance value.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.

# `union`

```elixir
-spec union(This, X, Y, Width, Height) -> boolean()
               when
                   This :: wxRegion(),
                   X :: integer(),
                   Y :: integer(),
                   Width :: integer(),
                   Height :: integer().
```

Finds the union of this region and another, rectangular region, specified using position
and size.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.

# `Xor`

```elixir
-spec 'Xor'(This, Rect) -> boolean()
               when
                   This :: wxRegion(),
                   Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()};
           (This, Region) -> boolean() when This :: wxRegion(), Region :: wxRegion().
```

Finds the Xor of this region and another region.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given `region`.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region, except for any overlapping areas. The result is stored in this region.

# `Xor`

```elixir
-spec 'Xor'(This, X, Y, Width, Height) -> boolean()
               when
                   This :: wxRegion(),
                   X :: integer(),
                   Y :: integer(),
                   Width :: integer(),
                   Height :: integer().
```

Finds the Xor of this region and another, rectangular region, specified using position
and size.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region, except for any overlapping areas. The result is stored in this region.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
