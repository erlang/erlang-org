# `wxJoystickEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxJoystickEvent.erl#L58)

This event class contains information about joystick events, particularly events received
by windows.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxJoystickEvent](https://docs.wxwidgets.org/3.2/classwx_joystick_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxJoystickEventType` to subscribe to events of this type.

# `wxJoystick`

```elixir
-type wxJoystick() ::
          #wxJoystick{type :: wxJoystickEvent:wxJoystickEventType(),
                      pos :: {X :: integer(), Y :: integer()},
                      zPosition :: integer(),
                      buttonChange :: integer(),
                      buttonState :: integer(),
                      joyStick :: integer()}.
```

# `wxJoystickEvent`

```elixir
-type wxJoystickEvent() :: wx:wx_object().
```

# `wxJoystickEventType`

```elixir
-type wxJoystickEventType() :: joy_button_down | joy_button_up | joy_move | joy_zmove.
```

# `buttonDown`

```elixir
-spec buttonDown(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonDown`

```elixir
-spec buttonDown(This, [Option]) -> boolean() when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the event was a down event from the specified button (or any button).

# `buttonIsDown`

```elixir
-spec buttonIsDown(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonIsDown`

```elixir
-spec buttonIsDown(This, [Option]) -> boolean()
                      when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the specified button (or any button) was in a down state.

# `buttonUp`

```elixir
-spec buttonUp(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonUp`

```elixir
-spec buttonUp(This, [Option]) -> boolean() when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the event was an up event from the specified button (or any button).

# `getButtonChange`

```elixir
-spec getButtonChange(This) -> integer() when This :: wxJoystickEvent().
```

Returns the identifier of the button changing state.

The return value is where `n` is the index of the button changing state, which can also
be retrieved using `GetButtonOrdinal()` (not implemented in wx).

Note that for `n` equal to 1, 2, 3 or 4 there are predefined `wxJOY_BUTTONn` constants
which can be used for more clarity, however these constants are not defined for the
buttons beyond the first four.

# `getButtonState`

```elixir
-spec getButtonState(This) -> integer() when This :: wxJoystickEvent().
```

Returns the down state of the buttons.

This is a `wxJOY_BUTTONn` identifier, where `n` is one of 1, 2, 3, 4.

# `getJoystick`

```elixir
-spec getJoystick(This) -> integer() when This :: wxJoystickEvent().
```

Returns the identifier of the joystick generating the event - one of wxJOYSTICK1 and
wxJOYSTICK2.

# `getPosition`

```elixir
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxJoystickEvent().
```

Returns the x, y position of the joystick event.

These coordinates are valid for all the events except wxEVT_JOY_ZMOVE.

# `getZPosition`

```elixir
-spec getZPosition(This) -> integer() when This :: wxJoystickEvent().
```

Returns the z position of the joystick event.

This method can only be used for wxEVT_JOY_ZMOVE events.

# `isButton`

```elixir
-spec isButton(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was a button up or down event (`not` 'is any button down?').

# `isMove`

```elixir
-spec isMove(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was an x, y move event.

# `isZMove`

```elixir
-spec isZMove(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was a z move event.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
