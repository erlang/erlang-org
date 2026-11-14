# `wxGridCellFloatRenderer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridCellFloatRenderer.erl#L58)

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

```erlang
-type wxGridCellFloatRenderer() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellFloatRenderer()) -> ok.
```

Destroys the object

# `getPrecision`

```erlang
-spec getPrecision(This) -> integer() when This :: wxGridCellFloatRenderer().
```

Returns the precision.

# `getWidth`

```erlang
-spec getWidth(This) -> integer() when This :: wxGridCellFloatRenderer().
```

Returns the width.

# `new`

```erlang
-spec new() -> wxGridCellFloatRenderer().
```

# `new`

```erlang
-spec new([Option]) -> wxGridCellFloatRenderer()
             when Option :: {width, integer()} | {precision, integer()} | {format, integer()}.
```

Float cell renderer ctor.

# `setParameters`

```erlang
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellFloatRenderer(), Params :: unicode:chardata().
```

The parameters string format is "width[,precision[,format]]" where `format` should be
chosen between f|e|g|E|G (f is used by default)

# `setPrecision`

```erlang
-spec setPrecision(This, Precision) -> ok when This :: wxGridCellFloatRenderer(), Precision :: integer().
```

Sets the precision.

# `setWidth`

```erlang
-spec setWidth(This, Width) -> ok when This :: wxGridCellFloatRenderer(), Width :: integer().
```

Sets the width.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
