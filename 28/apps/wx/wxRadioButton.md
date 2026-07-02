# `wxRadioButton`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxRadioButton.erl#L58)

A radio button item is a button which usually denotes one of several mutually exclusive
options.

It has a text label next to a (usually) round button.

You can create a group of mutually-exclusive radio buttons by specifying `wxRB_GROUP` for
the first in the group. The group ends when another radio button group is created, or
there are no more radio buttons.

## Styles

This class supports the following styles:

* wxRB_GROUP: Marks the beginning of a new group of radio buttons.

* wxRB_SINGLE: In some circumstances, radio buttons that are not consecutive siblings
trigger a hang bug in Windows (only). If this happens, add this style to mark the button
as not belonging to a group, and implement the mutually-exclusive group behaviour
yourself.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxRadioBox`

* `m:wxCheckBox`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxRadioButton](https://docs.wxwidgets.org/3.2/classwx_radio_button.html)

## Events

Event types emitted from this class:

* [`command_radiobutton_selected`](`m:wxCommandEvent`)

# `wxRadioButton`

```elixir
-type wxRadioButton() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxRadioButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxRadioButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the choice for two-step construction.

See `new/4` for further details.

# `destroy`

```elixir
-spec destroy(This :: wxRadioButton()) -> ok.
```

Destroys the object

# `getValue`

```elixir
-spec getValue(This) -> boolean() when This :: wxRadioButton().
```

Returns true if the radio button is checked, false otherwise.

# `new`

```elixir
-spec new() -> wxRadioButton().
```

Default constructor.

See: `create/5`

# `new`

```elixir
-spec new(Parent, Id, Label) -> wxRadioButton()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Label, [Option]) -> wxRadioButton()
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

Constructor, creating and showing a radio button.

See: `create/5`

# `setValue`

```elixir
-spec setValue(This, Value) -> ok when This :: wxRadioButton(), Value :: boolean().
```

Sets the radio button to checked or unchecked status.

This does not cause a `wxEVT_RADIOBUTTON` event to get emitted.

If the radio button belongs to a radio group exactly one button in the group may be
checked and so this method can be only called with `value` set to true. To uncheck a radio
button in a group you must check another button in the same group.

Note: Under MSW, the focused radio button is always selected, i.e. its value is true.
And, conversely, calling `SetValue(true)` will also set focus to the radio button if the
focus had previously been on another radio button in the same group - as otherwise setting
it on wouldn't work.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
