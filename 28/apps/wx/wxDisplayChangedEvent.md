# `wxDisplayChangedEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxDisplayChangedEvent.erl#L58)

A display changed event is sent to top-level windows when the display resolution has
changed.

This event is currently emitted under Windows only.

See: `m:wxDisplay`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxDisplayChangedEvent](https://docs.wxwidgets.org/3.2/classwx_display_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxDisplayChangedEventType` to subscribe to events of this type.

# `wxDisplayChanged`

```elixir
-type wxDisplayChanged() :: #wxDisplayChanged{type :: wxDisplayChangedEvent:wxDisplayChangedEventType()}.
```

# `wxDisplayChangedEvent`

```elixir
-type wxDisplayChangedEvent() :: wx:wx_object().
```

# `wxDisplayChangedEventType`

```elixir
-type wxDisplayChangedEventType() :: display_changed.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
