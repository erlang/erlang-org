# `wxGridCellNumberEditor`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGridCellNumberEditor.erl#L58)

Grid cell editor for numeric integer data.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellTextEditor`

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellNumberEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_number_editor.html)

# `wxGridCellNumberEditor`

```erlang
-type wxGridCellNumberEditor() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellNumberEditor()) -> ok.
```

Destroys the object

# `getValue`

```erlang
-spec getValue(This) -> unicode:charlist() when This :: wxGridCellNumberEditor().
```

Returns the value currently in the editor control.

# `new`

```erlang
-spec new() -> wxGridCellNumberEditor().
```

# `new`

```erlang
-spec new([Option]) -> wxGridCellNumberEditor() when Option :: {min, integer()} | {max, integer()}.
```

Allows you to specify the range for acceptable data.

Values equal to -1 for both `min` and `max` indicate that no range checking should be
done.

# `setParameters`

```erlang
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellNumberEditor(), Params :: unicode:chardata().
```

Parameters string format is "min,max".

---

*Consult [api-reference.md](api-reference.md) for complete listing*
