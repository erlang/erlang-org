# `wxMaximizeEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMaximizeEvent.erl#L58)

An event being sent when a top level window is maximized.

Notice that it is not sent when the window is restored to its original size after it had
been maximized, only a normal `m:wxSizeEvent` is generated in this case.

Currently this event is only generated in wxMSW, wxGTK and wxOSX/Cocoa ports so portable
programs should only rely on receiving `wxEVT_SIZE` and not necessarily this event when
the window is maximized.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxTopLevelWindow:maximize/2`

* `wxTopLevelWindow:isMaximized/1`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMaximizeEvent](https://docs.wxwidgets.org/3.2/classwx_maximize_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMaximizeEventType` to subscribe to events of this type.

# `wxMaximize`

```erlang
-type wxMaximize() :: #wxMaximize{type :: wxMaximizeEvent:wxMaximizeEventType()}.
```

# `wxMaximizeEvent`

```erlang
-type wxMaximizeEvent() :: wx:wx_object().
```

# `wxMaximizeEventType`

```erlang
-type wxMaximizeEventType() :: maximize.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
