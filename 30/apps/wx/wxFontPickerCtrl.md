# `wxFontPickerCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFontPickerCtrl.erl#L58)

This control allows the user to select a font.

The generic implementation is a button which brings up a `m:wxFontDialog` when clicked.
Native implementation may differ but this is usually a (small) widget which give access to
the font-chooser dialog. It is only available if `wxUSE_FONTPICKERCTRL` is set to 1 (the default).

## Styles

This class supports the following styles:

* wxFNTP_DEFAULT_STYLE: The default style: wxFNTP_FONTDESC_AS_LABEL |
wxFNTP_USEFONT_FOR_LABEL.

* wxFNTP_USE_TEXTCTRL: Creates a text control to the left of the picker button which is
completely managed by the `m:wxFontPickerCtrl` and which can be used by the user to
specify a font (see SetSelectedFont). The text control is automatically synchronized with
button's value. Use functions defined in `m:wxPickerBase` to modify the text control.

* wxFNTP_FONTDESC_AS_LABEL: Keeps the label of the button updated with the fontface name
and the font size. E.g. choosing "Times New Roman bold, italic with size 10" from the
fontdialog, will update the label (overwriting any previous label) with the "Times New
Roman, 10" text.

* wxFNTP_USEFONT_FOR_LABEL: Uses the currently selected font to draw the label of the
button.

See:
* `m:wxFontDialog`

* `m:wxFontPickerEvent`

This class is derived, and can use functions, from:

* `m:wxPickerBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFontPickerCtrl](https://docs.wxwidgets.org/3.2/classwx_font_picker_ctrl.html)

## Events

Event types emitted from this class:

* [`command_fontpicker_changed`](`m:wxFontPickerEvent`)

# `wxFontPickerCtrl`

```erlang
-type wxFontPickerCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxFontPickerCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxFontPickerCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {initial, wxFont:wxFont()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates this widget with given parameters.

Return: true if the control was successfully created or false if creation failed.

# `destroy`

```erlang
-spec destroy(This :: wxFontPickerCtrl()) -> ok.
```

Destroys the object

# `getMaxPointSize`

```erlang
-spec getMaxPointSize(This) -> integer() when This :: wxFontPickerCtrl().
```

Returns the maximum point size value allowed for the user-chosen font.

# `getSelectedFont`

```erlang
-spec getSelectedFont(This) -> wxFont:wxFont() when This :: wxFontPickerCtrl().
```

Returns the currently selected font.

Note that this function is completely different from `wxWindow:getFont/1`.

# `new`

```erlang
-spec new() -> wxFontPickerCtrl().
```

# `new`

```erlang
-spec new(Parent, Id) -> wxFontPickerCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxFontPickerCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {initial, wxFont:wxFont()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Initializes the object and calls `create/4` with all the parameters.

# `setMaxPointSize`

```erlang
-spec setMaxPointSize(This, Max) -> ok when This :: wxFontPickerCtrl(), Max :: integer().
```

Sets the maximum point size value allowed for the user-chosen font.

The default value is 100. Note that big fonts can require a lot of memory and CPU time
both for creation and for rendering; thus, specially because the user has the option to
specify the fontsize through a text control (see wxFNTP_USE_TEXTCTRL), it's a good idea to
put a limit to the maximum font size when huge fonts do not make much sense.

# `setSelectedFont`

```erlang
-spec setSelectedFont(This, Font) -> ok when This :: wxFontPickerCtrl(), Font :: wxFont:wxFont().
```

Sets the currently selected font.

Note that this function is completely different from `wxWindow:setFont/2`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
