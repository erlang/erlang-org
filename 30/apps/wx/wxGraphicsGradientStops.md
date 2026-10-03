# `wxGraphicsGradientStops`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsGradientStops.erl#L58)

Represents a collection of wxGraphicGradientStop values for use with
CreateLinearGradientBrush and CreateRadialGradientBrush.

The stops are maintained in order of position. If two or more stops are added with the
same position then the one(s) added later come later. This can be useful for producing
discontinuities in the colour gradient.

Notice that this class is write-once, you can't modify the stops once they had been added.

Since: 2.9.1

wxWidgets docs: [wxGraphicsGradientStops](https://docs.wxwidgets.org/3.2/classwx_graphics_gradient_stops.html)

# `wxGraphicsGradientStops`

```erlang
-type wxGraphicsGradientStops() :: wx:wx_object().
```

# `add`

```erlang
-spec add(This, Col, Pos) -> ok
             when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour(), Pos :: number().
```

Add a new stop.

# `destroy`

```erlang
-spec destroy(This :: wxGraphicsGradientStops()) -> ok.
```

Destroys the object

# `getCount`

```erlang
-spec getCount(This) -> integer() when This :: wxGraphicsGradientStops().
```

Returns the number of stops.

# `getEndColour`

```erlang
-spec getEndColour(This) -> wx:wx_colour4() when This :: wxGraphicsGradientStops().
```

Returns the end colour.

# `getStartColour`

```erlang
-spec getStartColour(This) -> wx:wx_colour4() when This :: wxGraphicsGradientStops().
```

Returns the start colour.

# `item`

```erlang
-spec item(This, N) -> {wx:wx_colour4(), float()} when This :: wxGraphicsGradientStops(), N :: integer().
```

Returns the stop at the given index.

# `new`

```erlang
-spec new() -> wxGraphicsGradientStops().
```

# `new`

```erlang
-spec new([Option]) -> wxGraphicsGradientStops()
             when Option :: {startCol, wx:wx_colour()} | {endCol, wx:wx_colour()}.
```

Initializes the gradient stops with the given boundary colours.

Creates a `m:wxGraphicsGradientStops` instance with start colour given by `startCol` and
end colour given by `endCol`.

# `setEndColour`

```erlang
-spec setEndColour(This, Col) -> ok when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour().
```

Set the end colour to `col`.

# `setStartColour`

```erlang
-spec setStartColour(This, Col) -> ok when This :: wxGraphicsGradientStops(), Col :: wx:wx_colour().
```

Set the start colour to `col`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
