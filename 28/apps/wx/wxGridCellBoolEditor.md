# `wxGridCellBoolEditor`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGridCellBoolEditor.erl#L58)

Grid cell editor for boolean data.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellBoolEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_bool_editor.html)

# `wxGridCellBoolEditor`

```elixir
-type wxGridCellBoolEditor() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxGridCellBoolEditor()) -> ok.
```

Destroys the object

# `isTrueValue`

```elixir
-spec isTrueValue(Value) -> boolean() when Value :: unicode:chardata().
```

Returns true if the given `value` is equal to the string representation of the truth
value we currently use (see `useStringValues/1`).

# `new`

```elixir
-spec new() -> wxGridCellBoolEditor().
```

Default constructor.

# `useStringValues`

```elixir
-spec useStringValues() -> ok.
```

# `useStringValues`

```elixir
-spec useStringValues([Option]) -> ok
                         when
                             Option ::
                                 {valueTrue, unicode:chardata()} | {valueFalse, unicode:chardata()}.
```

This method allows you to customize the values returned by `wxGridCellNumberEditor:getValue/1`
for the cell using this editor.

By default, the default values of the arguments are used, i.e. `"1"` is returned if the
cell is checked and an empty string otherwise.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
