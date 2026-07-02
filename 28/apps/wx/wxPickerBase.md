# `wxPickerBase`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPickerBase.erl#L58)

Base abstract class for all pickers which support an auxiliary text control.

This class handles all positioning and sizing of the text control like a an horizontal `m:wxBoxSizer`
would do, with the text control on the left of the picker button.

The proportion (see `m:wxSizer` documentation for more info about proportion values) of
the picker control defaults to 1 when there isn't a text control associated (see `wxPB_USE_TEXTCTRL`
style) and to 0 otherwise.

## Styles

This class supports the following styles:

* wxPB_USE_TEXTCTRL: Creates a text control to the left of the picker which is completely
managed by this `m:wxPickerBase` class.

See: `m:wxColourPickerCtrl`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPickerBase](https://docs.wxwidgets.org/3.2/classwx_picker_base.html)

# `wxPickerBase`

```elixir
-type wxPickerBase() :: wx:wx_object().
```

# `getInternalMargin`

```elixir
-spec getInternalMargin(This) -> integer() when This :: wxPickerBase().
```

Returns the margin (in pixel) between the picker and the text control.

This function can be used only when `hasTextCtrl/1` returns true.

# `getPickerCtrlProportion`

```elixir
-spec getPickerCtrlProportion(This) -> integer() when This :: wxPickerBase().
```

Returns the proportion value of the picker.

# `getTextCtrl`

```elixir
-spec getTextCtrl(This) -> wxTextCtrl:wxTextCtrl() when This :: wxPickerBase().
```

Returns a pointer to the text control handled by this window or NULL if the `wxPB\_USE\_TEXTCTRL`
style was not specified when this control was created.

Remark: The contents of the text control could be an invalid representation of the entity
which can be chosen through the picker (e.g. when the user enters an invalid colour syntax
because of a typo). Thus you should never parse the content of the textctrl to get the
user's input; rather use the derived-class getter (e.g. `wxColourPickerCtrl:getColour/1`, `wxFilePickerCtrl:getPath/1`, etc).

# `getTextCtrlProportion`

```elixir
-spec getTextCtrlProportion(This) -> integer() when This :: wxPickerBase().
```

Returns the proportion value of the text control.

This function can be used only when `hasTextCtrl/1` returns true.

# `hasTextCtrl`

```elixir
-spec hasTextCtrl(This) -> boolean() when This :: wxPickerBase().
```

Returns true if this window has a valid text control (i.e. if the `wxPB\_USE\_TEXTCTRL`
style was given when creating this control).

# `isPickerCtrlGrowable`

```elixir
-spec isPickerCtrlGrowable(This) -> boolean() when This :: wxPickerBase().
```

Returns true if the picker control is growable.

# `isTextCtrlGrowable`

```elixir
-spec isTextCtrlGrowable(This) -> boolean() when This :: wxPickerBase().
```

Returns true if the text control is growable.

This function can be used only when `hasTextCtrl/1` returns true.

# `setInternalMargin`

```elixir
-spec setInternalMargin(This, Margin) -> ok when This :: wxPickerBase(), Margin :: integer().
```

Sets the margin (in pixel) between the picker and the text control.

This function can be used only when `hasTextCtrl/1` returns true.

# `setPickerCtrlGrowable`

```elixir
-spec setPickerCtrlGrowable(This) -> ok when This :: wxPickerBase().
```

# `setPickerCtrlGrowable`

```elixir
-spec setPickerCtrlGrowable(This, [Option]) -> ok
                               when This :: wxPickerBase(), Option :: {grow, boolean()}.
```

Sets the picker control as growable when `grow` is true.

# `setPickerCtrlProportion`

```elixir
-spec setPickerCtrlProportion(This, Prop) -> ok when This :: wxPickerBase(), Prop :: integer().
```

Sets the proportion value of the picker.

Look at the detailed description of `m:wxPickerBase` for more info.

# `setTextCtrlGrowable`

```elixir
-spec setTextCtrlGrowable(This) -> ok when This :: wxPickerBase().
```

# `setTextCtrlGrowable`

```elixir
-spec setTextCtrlGrowable(This, [Option]) -> ok when This :: wxPickerBase(), Option :: {grow, boolean()}.
```

Sets the text control as growable when `grow` is true.

This function can be used only when `hasTextCtrl/1` returns true.

# `setTextCtrlProportion`

```elixir
-spec setTextCtrlProportion(This, Prop) -> ok when This :: wxPickerBase(), Prop :: integer().
```

Sets the proportion value of the text control.

Look at the detailed description of `m:wxPickerBase` for more info.

This function can be used only when `hasTextCtrl/1` returns true.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
