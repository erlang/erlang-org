# `wxGridCellStringRenderer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridCellStringRenderer.erl#L58)

This class may be used to format string data in a cell; it is the default for string
cells.

See:
* `m:wxGridCellRenderer`

* `m:wxGridCellBoolRenderer`

* `m:wxGridCellFloatRenderer`

* `m:wxGridCellNumberRenderer`

This class is derived, and can use functions, from:

* `m:wxGridCellRenderer`

wxWidgets docs: [wxGridCellStringRenderer](https://docs.wxwidgets.org/3.2/classwx_grid_cell_string_renderer.html)

# `wxGridCellStringRenderer`

```erlang
-type wxGridCellStringRenderer() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellStringRenderer()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxGridCellStringRenderer().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
