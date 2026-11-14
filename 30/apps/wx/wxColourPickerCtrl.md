# `wxColourPickerCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxColourPickerCtrl.erl#L58)

This control allows the user to select a colour.

The generic implementation is a button which brings up a `m:wxColourDialog` when clicked.
Native implementation may differ but this is usually a (small) widget which give access to
the colour-chooser dialog. It is only available if `wxUSE_COLOURPICKERCTRL` is set to 1
(the default).

## Styles

This class supports the following styles:

* wxCLRP_DEFAULT_STYLE: The default style: 0.

* wxCLRP_USE_TEXTCTRL: Creates a text control to the left of the picker button which is
completely managed by the `m:wxColourPickerCtrl` and which can be used by the user to
specify a colour (see SetColour). The text control is automatically synchronized with
button's value. Use functions defined in `m:wxPickerBase` to modify the text control.

* wxCLRP_SHOW_LABEL: Shows the colour in HTML form (AABBCC) as colour button label (instead
of no label at all).

* wxCLRP_SHOW_ALPHA: Allows selecting opacity in the colour-chooser (effective under wxGTK
and wxOSX).

See:
* `m:wxColourDialog`

* `m:wxColourPickerEvent`

This class is derived, and can use functions, from:

* `m:wxPickerBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxColourPickerCtrl](https://docs.wxwidgets.org/3.2/classwx_colour_picker_ctrl.html)

## Events

Event types emitted from this class:

* [`command_colourpicker_changed`](`m:wxColourPickerEvent`)

# `wxColourPickerCtrl`

```erlang
-type wxColourPickerCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxColourPickerCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxColourPickerCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {col, wx:wx_colour()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates a colour picker with the given arguments.

Return: true if the control was successfully created or false if creation failed.

# `destroy`

```erlang
-spec destroy(This :: wxColourPickerCtrl()) -> ok.
```

Destroys the object

# `getColour`

```erlang
-spec getColour(This) -> wx:wx_colour4() when This :: wxColourPickerCtrl().
```

Returns the currently selected colour.

# `new`

```erlang
-spec new() -> wxColourPickerCtrl().
```

# `new`

```erlang
-spec new(Parent, Id) -> wxColourPickerCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxColourPickerCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {col, wx:wx_colour()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Initializes the object and calls `create/4` with all the parameters.

# `setColour`

```erlang
-spec setColour(This, Colname) -> ok when This :: wxColourPickerCtrl(), Colname :: unicode:chardata();
               (This, Col) -> ok when This :: wxColourPickerCtrl(), Col :: wx:wx_colour().
```

Sets the currently selected colour.

See `wxColour::Set()` (not implemented in wx).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
