# `wxEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxEvent.erl#L58)

An event is a structure holding information about an event passed to a callback or member
function.

`m:wxEvent` used to be a multipurpose event object, and is an abstract base class for
other event classes (see below).

For more information about events, see the overview_events overview.

See:
* `m:wxCommandEvent`

* `m:wxMouseEvent`

wxWidgets docs: [wxEvent](https://docs.wxwidgets.org/3.2/classwx_event.html)

# `wxEvent`

```elixir
-type wxEvent() :: wx:wx_object().
```

# `getId`

```elixir
-spec getId(This) -> integer() when This :: wxEvent().
```

Returns the identifier associated with this event, such as a button command id.

# `getSkipped`

```elixir
-spec getSkipped(This) -> boolean() when This :: wxEvent().
```

Returns true if the event handler should be skipped, false otherwise.

# `getTimestamp`

```elixir
-spec getTimestamp(This) -> integer() when This :: wxEvent().
```

Gets the timestamp for the event.

The timestamp is the time in milliseconds since some fixed moment (not necessarily the
standard Unix Epoch, so only differences between the timestamps and not their absolute
values usually make sense).

Warning:

wxWidgets returns a non-NULL timestamp only for mouse and key events (see `m:wxMouseEvent`
and `m:wxKeyEvent`).

# `isCommandEvent`

```elixir
-spec isCommandEvent(This) -> boolean() when This :: wxEvent().
```

Returns true if the event is or is derived from `m:wxCommandEvent` else it returns false.

Note: exists only for optimization purposes.

# `resumePropagation`

```elixir
-spec resumePropagation(This, PropagationLevel) -> ok
                           when This :: wxEvent(), PropagationLevel :: integer().
```

Sets the propagation level to the given value (for example returned from an earlier call
to `stopPropagation/1`).

# `shouldPropagate`

```elixir
-spec shouldPropagate(This) -> boolean() when This :: wxEvent().
```

Test if this event should be propagated or not, i.e. if the propagation level is
currently greater than 0.

# `skip`

```elixir
-spec skip(This) -> ok when This :: wxEvent().
```

# `skip`

```elixir
-spec skip(This, [Option]) -> ok when This :: wxEvent(), Option :: {skip, boolean()}.
```

This method can be used inside an event handler to control whether further event handlers
bound to this event will be called after the current one returns.

Without `skip/2` (or equivalently if Skip(false) is used), the event will not be processed any
more. If Skip(true) is called, the event processing system continues searching for a
further handler function for this event, even though it has been processed already in the
current handler.

In general, it is recommended to skip all non-command events to allow the default
handling to take place. The command events are, however, normally not skipped as usually a
single command such as a button click or menu item selection must only be processed by one
handler.

# `stopPropagation`

```elixir
-spec stopPropagation(This) -> integer() when This :: wxEvent().
```

Stop the event from propagating to its parent window.

Returns the old propagation level value which may be later passed to `resumePropagation/2` to allow
propagating the event again.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
