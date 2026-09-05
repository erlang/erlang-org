# `wxShowEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxShowEvent.erl#L58)

An event being sent when the window is shown or hidden.

The event is triggered by calls to `wxWindow:show/2`, and any user action showing a previously hidden
window or vice versa (if allowed by the current platform and/or window manager). Notice
that the event is not triggered when the application is iconized (minimized) or restored
under wxMSW.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:show/2`

* `wxWindow:isShown/1`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxShowEvent](https://docs.wxwidgets.org/3.2/classwx_show_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxShowEventType` to subscribe to events of this type.

# `wxShow`

```erlang
-type wxShow() :: #wxShow{type :: wxShowEvent:wxShowEventType(), show :: boolean()}.
```

# `wxShowEvent`

```erlang
-type wxShowEvent() :: wx:wx_object().
```

# `wxShowEventType`

```erlang
-type wxShowEventType() :: show.
```

# `isShown`

```erlang
-spec isShown(This) -> boolean() when This :: wxShowEvent().
```

Return true if the window has been shown, false if it has been hidden.

# `setShow`

```erlang
-spec setShow(This, Show) -> ok when This :: wxShowEvent(), Show :: boolean().
```

Set whether the windows was shown or hidden.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
