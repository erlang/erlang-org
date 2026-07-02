# `wxActivateEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxActivateEvent.erl#L58)

An activate event is sent when a window or application is being activated or deactivated.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxActivateEvent](https://docs.wxwidgets.org/3.2/classwx_activate_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxActivateEventType` to subscribe to events of this type.

# `wxActivate`

```elixir
-type wxActivate() :: #wxActivate{type :: wxActivateEvent:wxActivateEventType(), active :: boolean()}.
```

# `wxActivateEvent`

```elixir
-type wxActivateEvent() :: wx:wx_object().
```

# `wxActivateEventType`

```elixir
-type wxActivateEventType() :: activate | activate_app | hibernate.
```

# `getActive`

```elixir
-spec getActive(This) -> boolean() when This :: wxActivateEvent().
```

Returns true if the application or window is being activated, false otherwise.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
