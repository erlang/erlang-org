# `wxGridCellFloatEditor`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGridCellFloatEditor.erl#L58)

The editor for floating point numbers data.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellBoolEditor`

* `m:wxGridCellChoiceEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellFloatEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_float_editor.html)

# `wxGridCellFloatEditor`

```erlang
-type wxGridCellFloatEditor() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellFloatEditor()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxGridCellFloatEditor().
```

# `new`

```erlang
-spec new([Option]) -> wxGridCellFloatEditor()
             when Option :: {width, integer()} | {precision, integer()} | {format, integer()}.
```

Float cell editor ctor.

# `setParameters`

```erlang
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellFloatEditor(), Params :: unicode:chardata().
```

The parameters string format is "width[,precision[,format]]" where `format` should be
chosen between f|e|g|E|G (f is used by default)

---

*Consult [api-reference.md](api-reference.md) for complete listing*
