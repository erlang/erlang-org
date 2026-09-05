# `wxWindowCreateEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxWindowCreateEvent.erl#L58)

This event is sent just after the actual window associated with a `m:wxWindow` object has
been created.

Since it is derived from `m:wxCommandEvent`, the event propagates up the window hierarchy.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxWindowDestroyEvent`

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxWindowCreateEvent](https://docs.wxwidgets.org/3.2/classwx_window_create_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxWindowCreateEventType` to subscribe to events of this type.

# `wxWindowCreate`

```erlang
-type wxWindowCreate() :: #wxWindowCreate{type :: wxWindowCreateEvent:wxWindowCreateEventType()}.
```

# `wxWindowCreateEvent`

```erlang
-type wxWindowCreateEvent() :: wx:wx_object().
```

# `wxWindowCreateEventType`

```erlang
-type wxWindowCreateEventType() :: create.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
