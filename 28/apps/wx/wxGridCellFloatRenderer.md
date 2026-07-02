# `wxGridCellFloatRenderer`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGridCellFloatRenderer.erl#L58)

This class may be used to format floating point data in a cell.

See:
* `m:wxGridCellRenderer`

* `m:wxGridCellBoolRenderer`

* `m:wxGridCellNumberRenderer`

* `m:wxGridCellStringRenderer`

This class is derived, and can use functions, from:

* `m:wxGridCellStringRenderer`

* `m:wxGridCellRenderer`

wxWidgets docs: [wxGridCellFloatRenderer](https://docs.wxwidgets.org/3.2/classwx_grid_cell_float_renderer.html)

# `wxGridCellFloatRenderer`

```elixir
-type wxGridCellFloatRenderer() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxGridCellFloatRenderer()) -> ok.
```

Destroys the object

# `getPrecision`

```elixir
-spec getPrecision(This) -> integer() when This :: wxGridCellFloatRenderer().
```

Returns the precision.

# `getWidth`

```elixir
-spec getWidth(This) -> integer() when This :: wxGridCellFloatRenderer().
```

Returns the width.

# `new`

```elixir
-spec new() -> wxGridCellFloatRenderer().
```

# `new`

```elixir
-spec new([Option]) -> wxGridCellFloatRenderer()
             when Option :: {width, integer()} | {precision, integer()} | {format, integer()}.
```

Float cell renderer ctor.

# `setParameters`

```elixir
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellFloatRenderer(), Params :: unicode:chardata().
```

The parameters string format is "width[,precision[,format]]" where `format` should be
chosen between f|e|g|E|G (f is used by default)

# `setPrecision`

```elixir
-spec setPrecision(This, Precision) -> ok when This :: wxGridCellFloatRenderer(), Precision :: integer().
```

Sets the precision.

# `setWidth`

```elixir
-spec setWidth(This, Width) -> ok when This :: wxGridCellFloatRenderer(), Width :: integer().
```

Sets the width.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
