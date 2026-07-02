# `wxSetCursorEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxSetCursorEvent.erl#L58)

A `m:wxSetCursorEvent` is generated from `m:wxWindow` when the mouse cursor is about to
be set as a result of mouse motion.

This event gives the application the chance to perform specific mouse cursor processing
based on the current position of the mouse within the window. Use `setCursor/2` to specify the cursor
you want to be displayed.

See:
* `wx_misc:setCursor/1`

* `wxWindow:setCursor/2`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxSetCursorEvent](https://docs.wxwidgets.org/3.2/classwx_set_cursor_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxSetCursorEventType` to subscribe to events of this type.

# `wxSetCursor`

```elixir
-type wxSetCursor() ::
          #wxSetCursor{type :: wxSetCursorEvent:wxSetCursorEventType(),
                       x :: integer(),
                       y :: integer(),
                       cursor :: wxCursor:wxCursor()}.
```

# `wxSetCursorEvent`

```elixir
-type wxSetCursorEvent() :: wx:wx_object().
```

# `wxSetCursorEventType`

```elixir
-type wxSetCursorEventType() :: set_cursor.
```

# `getCursor`

```elixir
-spec getCursor(This) -> wxCursor:wxCursor() when This :: wxSetCursorEvent().
```

Returns a reference to the cursor specified by this event.

# `getX`

```elixir
-spec getX(This) -> integer() when This :: wxSetCursorEvent().
```

Returns the X coordinate of the mouse in client coordinates.

# `getY`

```elixir
-spec getY(This) -> integer() when This :: wxSetCursorEvent().
```

Returns the Y coordinate of the mouse in client coordinates.

# `hasCursor`

```elixir
-spec hasCursor(This) -> boolean() when This :: wxSetCursorEvent().
```

Returns true if the cursor specified by this event is a valid cursor.

Remark: You cannot specify wxNullCursor with this event, as it is not considered a valid
cursor.

# `setCursor`

```elixir
-spec setCursor(This, Cursor) -> ok when This :: wxSetCursorEvent(), Cursor :: wxCursor:wxCursor().
```

Sets the cursor associated with this event.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
