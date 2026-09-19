# `wxActivateEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxActivateEvent.erl#L58)

An activate event is sent when a window or application is being activated or deactivated.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxActivateEvent](https://docs.wxwidgets.org/3.2/classwx_activate_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxActivateEventType` to subscribe to events of this type.

# `wxActivate`

```erlang
-type wxActivate() :: #wxActivate{type :: wxActivateEvent:wxActivateEventType(), active :: boolean()}.
```

# `wxActivateEvent`

```erlang
-type wxActivateEvent() :: wx:wx_object().
```

# `wxActivateEventType`

```erlang
-type wxActivateEventType() :: activate | activate_app | hibernate.
```

# `getActive`

```erlang
-spec getActive(This) -> boolean() when This :: wxActivateEvent().
```

Returns true if the application or window is being activated, false otherwise.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
