# `wxToggleButton`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxToggleButton.erl#L58)

`m:wxToggleButton` is a button that stays pressed when clicked by the user.

In other words, it is similar to `m:wxCheckBox` in functionality but looks like a `m:wxButton`.

Since wxWidgets version 2.9.0 this control emits an update UI event.

You can see `m:wxToggleButton` in action in page_samples_widgets.

See:
* `m:wxCheckBox`

* `m:wxButton`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxToggleButton](https://docs.wxwidgets.org/3.2/classwx_toggle_button.html)

## Events

Event types emitted from this class:

* [`command_togglebutton_clicked`](`m:wxCommandEvent`)

# `wxToggleButton`

```elixir
-type wxToggleButton() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxToggleButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxToggleButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the toggle button for two-step construction.

See `new/4` for details.

# `destroy`

```elixir
-spec destroy(This :: wxToggleButton()) -> ok.
```

Destroys the object

# `getValue`

```elixir
-spec getValue(This) -> boolean() when This :: wxToggleButton().
```

Gets the state of the toggle button.

Return: Returns true if it is pressed, false otherwise.

# `new`

```elixir
-spec new() -> wxToggleButton().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id, Label) -> wxToggleButton()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Label, [Option]) -> wxToggleButton()
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

Constructor, creating and showing a toggle button.

See: `create/5`

# `setValue`

```elixir
-spec setValue(This, State) -> ok when This :: wxToggleButton(), State :: boolean().
```

Sets the toggle button to the given state.

This does not cause a `EVT_TOGGLEBUTTON` event to be emitted.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
