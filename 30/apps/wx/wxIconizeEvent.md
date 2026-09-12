# `wxIconizeEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxIconizeEvent.erl#L58)

An event being sent when the frame is iconized (minimized) or restored.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxTopLevelWindow:iconize/2`

* `wxTopLevelWindow:isIconized/1`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxIconizeEvent](https://docs.wxwidgets.org/3.2/classwx_iconize_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxIconizeEventType` to subscribe to events of this type.

# `wxIconize`

```erlang
-type wxIconize() :: #wxIconize{type :: wxIconizeEvent:wxIconizeEventType(), iconized :: boolean()}.
```

# `wxIconizeEvent`

```erlang
-type wxIconizeEvent() :: wx:wx_object().
```

# `wxIconizeEventType`

```erlang
-type wxIconizeEventType() :: iconize.
```

# `isIconized`

```erlang
-spec isIconized(This) -> boolean() when This :: wxIconizeEvent().
```

Returns true if the frame has been iconized, false if it has been restored.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
