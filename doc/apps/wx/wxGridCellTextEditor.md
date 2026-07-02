# `wxGridCellTextEditor`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGridCellTextEditor.erl#L58)

Grid cell editor for string/text data.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellTextEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_text_editor.html)

# `wxGridCellTextEditor`

```erlang
-type wxGridCellTextEditor() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellTextEditor()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxGridCellTextEditor().
```

# `new`

```erlang
-spec new([Option]) -> wxGridCellTextEditor() when Option :: {maxChars, integer()}.
```

Text cell editor constructor.

# `setParameters`

```erlang
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellTextEditor(), Params :: unicode:chardata().
```

The parameters string format is "n" where n is a number representing the maximum width.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
