# `wxJoystickEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxJoystickEvent.erl#L58)

This event class contains information about joystick events, particularly events received
by windows.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxJoystickEvent](https://docs.wxwidgets.org/3.2/classwx_joystick_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxJoystickEventType` to subscribe to events of this type.

# `wxJoystick`

```erlang
-type wxJoystick() ::
          #wxJoystick{type :: wxJoystickEvent:wxJoystickEventType(),
                      pos :: {X :: integer(), Y :: integer()},
                      zPosition :: integer(),
                      buttonChange :: integer(),
                      buttonState :: integer(),
                      joyStick :: integer()}.
```

# `wxJoystickEvent`

```erlang
-type wxJoystickEvent() :: wx:wx_object().
```

# `wxJoystickEventType`

```erlang
-type wxJoystickEventType() :: joy_button_down | joy_button_up | joy_move | joy_zmove.
```

# `buttonDown`

```erlang
-spec buttonDown(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonDown`

```erlang
-spec buttonDown(This, [Option]) -> boolean() when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the event was a down event from the specified button (or any button).

# `buttonIsDown`

```erlang
-spec buttonIsDown(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonIsDown`

```erlang
-spec buttonIsDown(This, [Option]) -> boolean()
                      when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the specified button (or any button) was in a down state.

# `buttonUp`

```erlang
-spec buttonUp(This) -> boolean() when This :: wxJoystickEvent().
```

# `buttonUp`

```erlang
-spec buttonUp(This, [Option]) -> boolean() when This :: wxJoystickEvent(), Option :: {but, integer()}.
```

Returns true if the event was an up event from the specified button (or any button).

# `getButtonChange`

```erlang
-spec getButtonChange(This) -> integer() when This :: wxJoystickEvent().
```

Returns the identifier of the button changing state.

The return value is where `n` is the index of the button changing state, which can also
be retrieved using `GetButtonOrdinal()` (not implemented in wx).

Note that for `n` equal to 1, 2, 3 or 4 there are predefined `wxJOY_BUTTONn` constants
which can be used for more clarity, however these constants are not defined for the
buttons beyond the first four.

# `getButtonState`

```erlang
-spec getButtonState(This) -> integer() when This :: wxJoystickEvent().
```

Returns the down state of the buttons.

This is a `wxJOY_BUTTONn` identifier, where `n` is one of 1, 2, 3, 4.

# `getJoystick`

```erlang
-spec getJoystick(This) -> integer() when This :: wxJoystickEvent().
```

Returns the identifier of the joystick generating the event - one of wxJOYSTICK1 and
wxJOYSTICK2.

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxJoystickEvent().
```

Returns the x, y position of the joystick event.

These coordinates are valid for all the events except wxEVT_JOY_ZMOVE.

# `getZPosition`

```erlang
-spec getZPosition(This) -> integer() when This :: wxJoystickEvent().
```

Returns the z position of the joystick event.

This method can only be used for wxEVT_JOY_ZMOVE events.

# `isButton`

```erlang
-spec isButton(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was a button up or down event (`not` 'is any button down?').

# `isMove`

```erlang
-spec isMove(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was an x, y move event.

# `isZMove`

```erlang
-spec isZMove(This) -> boolean() when This :: wxJoystickEvent().
```

Returns true if this was a z move event.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
