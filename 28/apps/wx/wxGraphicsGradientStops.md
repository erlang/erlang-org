# `wxGraphicsGradientStops`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGraphicsGradientStops.erl#L58)

Represents a collection of wxGraphicGradientStop values for use with
CreateLinearGradientBrush and CreateRadialGradientBrush.

The stops are maintained in order of position. If two or more stops are added with the
same position then the one(s) added later come later. This can be useful for producing
discontinuities in the colour gradient.

Notice that this class is write-once, you can't modify the stops once they had been added.

Since: 2.9.1

wxWidgets docs: [wxGraphicsGradientStops](https://docs.wxwidgets.org/3.2/classwx_graphics_gradient_stops.html)

# `wxGraphicsGradientStops`

```elixir
-type wxGraphicsGradientStops() :: wx:wx_object().
```

# `add`

```elixir
-spec add(This, Col, Pos) -> ok
             when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour(), Pos :: number().
```

Add a new stop.

# `destroy`

```elixir
-spec destroy(This :: wxGraphicsGradientStops()) -> ok.
```

Destroys the object

# `getCount`

```elixir
-spec getCount(This) -> integer() when This :: wxGraphicsGradientStops().
```

Returns the number of stops.

# `getEndColour`

```elixir
-spec getEndColour(This) -> wx:wx_colour4() when This :: wxGraphicsGradientStops().
```

Returns the end colour.

# `getStartColour`

```elixir
-spec getStartColour(This) -> wx:wx_colour4() when This :: wxGraphicsGradientStops().
```

Returns the start colour.

# `item`

```elixir
-spec item(This, N) -> {wx:wx_colour4(), float()} when This :: wxGraphicsGradientStops(), N :: integer().
```

Returns the stop at the given index.

# `new`

```elixir
-spec new() -> wxGraphicsGradientStops().
```

# `new`

```elixir
-spec new([Option]) -> wxGraphicsGradientStops()
             when Option :: {startCol, wx:wx_colour()} | {endCol, wx:wx_colour()}.
```

Initializes the gradient stops with the given boundary colours.

Creates a `m:wxGraphicsGradientStops` instance with start colour given by `startCol` and
end colour given by `endCol`.

# `setEndColour`

```elixir
-spec setEndColour(This, Col) -> ok when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour().
```

Set the end colour to `col`.

# `setStartColour`

```elixir
-spec setStartColour(This, Col) -> ok when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour().
```

Set the start colour to `col`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
