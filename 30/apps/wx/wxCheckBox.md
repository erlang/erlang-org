# `wxCheckBox`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCheckBox.erl#L58)

A checkbox is a labelled box which by default is either on (checkmark is visible) or off
(no checkmark).

Optionally (when the wxCHK_3STATE style flag is set) it can have a third state, called
the mixed or undetermined state. Often this is used as a "Does Not Apply" state.

## Styles

This class supports the following styles:

* wxCHK_2STATE: Create a 2-state checkbox. This is the default.

* wxCHK_3STATE: Create a 3-state checkbox. Not implemented in wxGTK1.

* wxCHK_ALLOW_3RD_STATE_FOR_USER: By default a user can't set a 3-state checkbox to the
third state. It can only be done from code. Using this flags allows the user to set the
checkbox to the third state by clicking.

* wxALIGN_RIGHT: Makes the text appear on the left of the checkbox.

See:
* `m:wxRadioButton`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxCheckBox](https://docs.wxwidgets.org/3.2/classwx_check_box.html)

## Events

Event types emitted from this class:

* [`command_checkbox_clicked`](`m:wxCommandEvent`)

# `wxCheckBox`

```erlang
-type wxCheckBox() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxCheckBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxCheckBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the checkbox for two-step construction.

See `new/4` for details.

# `destroy`

```erlang
-spec destroy(This :: wxCheckBox()) -> ok.
```

Destroys the object

# `get3StateValue`

```erlang
-spec get3StateValue(This) -> wx:wx_enum() when This :: wxCheckBox().
```

Gets the state of a 3-state checkbox.

Asserts when the function is used with a 2-state checkbox.

# `getValue`

```erlang
-spec getValue(This) -> boolean() when This :: wxCheckBox().
```

Gets the state of a 2-state checkbox.

Return: Returns true if it is checked, false otherwise.

# `is3rdStateAllowedForUser`

```erlang
-spec is3rdStateAllowedForUser(This) -> boolean() when This :: wxCheckBox().
```

Returns whether or not the user can set the checkbox to the third state.

Return: true if the user can set the third state of this checkbox, false if it can only
be set programmatically or if it's a 2-state checkbox.

# `is3State`

```erlang
-spec is3State(This) -> boolean() when This :: wxCheckBox().
```

Returns whether or not the checkbox is a 3-state checkbox.

Return: true if this checkbox is a 3-state checkbox, false if it's a 2-state checkbox.

# `isChecked`

```erlang
-spec isChecked(This) -> boolean() when This :: wxCheckBox().
```

This is just a maybe more readable synonym for `getValue/1`: just as the latter, it
returns true if the checkbox is checked and false otherwise.

# `new`

```erlang
-spec new() -> wxCheckBox().
```

Default constructor.

See: `create/5`

# `new`

```erlang
-spec new(Parent, Id, Label) -> wxCheckBox()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: unicode:chardata().
```

# `new`

```erlang
-spec new(Parent, Id, Label, [Option]) -> wxCheckBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a checkbox.

See: `create/5`

# `set3StateValue`

```erlang
-spec set3StateValue(This, State) -> ok when This :: wxCheckBox(), State :: wx:wx_enum().
```

Sets the checkbox to the given state.

This does not cause a `wxEVT_CHECKBOX` event to get emitted.

Asserts when the checkbox is a 2-state checkbox and setting the state to
wxCHK_UNDETERMINED.

# `setValue`

```erlang
-spec setValue(This, State) -> ok when This :: wxCheckBox(), State :: boolean().
```

Sets the checkbox to the given state.

This does not cause a `wxEVT_CHECKBOX` event to get emitted.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
