# `wxGridCellChoiceEditor`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGridCellChoiceEditor.erl#L58)

Grid cell editor for string data providing the user a choice from a list of strings.

See:
* `m:wxGridCellEditor`

* `m:wxGridCellBoolEditor`

* `m:wxGridCellFloatEditor`

* `m:wxGridCellNumberEditor`

* `m:wxGridCellTextEditor`

This class is derived, and can use functions, from:

* `m:wxGridCellEditor`

wxWidgets docs: [wxGridCellChoiceEditor](https://docs.wxwidgets.org/3.2/classwx_grid_cell_choice_editor.html)

# `wxGridCellChoiceEditor`

```erlang
-type wxGridCellChoiceEditor() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGridCellChoiceEditor()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Choices) -> wxGridCellChoiceEditor() when Choices :: [unicode:chardata()].
```

# `new`

```erlang
-spec new(Choices, [Option]) -> wxGridCellChoiceEditor()
             when Choices :: [unicode:chardata()], Option :: {allowOthers, boolean()}.
```

Choice cell renderer ctor.

# `setParameters`

```erlang
-spec setParameters(This, Params) -> ok
                       when This :: wxGridCellChoiceEditor(), Params :: unicode:chardata().
```

Parameters string format is "item1[,item2[...,itemN]]".

This method can be called before the editor is used for the first time, or later, in
which case it replaces the previously specified strings with the new ones.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
