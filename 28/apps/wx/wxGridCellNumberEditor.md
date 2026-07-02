# `wxGridCellNumberEditor`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGridCellNumberEditor.erl#L58)

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

```elixir
-type wxGridCellNumberEditor() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxGridCellNumberEditor()) -> ok.
```

Destroys the object

# `getValue`

```elixir
-spec getValue(This) -> unicode:charlist() when This :: wxGridCellNumberEditor().
```

Returns the value currently in the editor control.

# `new`

```elixir
-spec new() -> wxGridCellNumberEditor().
```

# `new`

```elixir
-spec new([Option]) -> wxGridCellNumberEditor() when Option :: {min, integer()} | {max, integer()}.
```

Allows you to specify the range for acceptable data.

Values equal to -1 for both `min` and `max` indicate that no range checking should be
done.

# `setParameters`

```elixir
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellNumberEditor(), Params :: unicode:chardata().
```

Parameters string format is "min,max".

---

*Consult [api-reference.md](api-reference.md) for complete listing*
