# `wxSysColourChangedEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxSysColourChangedEvent.erl#L58)

This class is used for system colour change events, which are generated when the user
changes the colour settings using the control panel.

This is only appropriate under Windows.

Remark: The default event handler for this event propagates the event to child windows,
since Windows only sends the events to top-level windows. If intercepting this event for a
top-level window, remember to call the base class handler, or to pass the event on to the
window's children explicitly.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxSysColourChangedEvent](https://docs.wxwidgets.org/3.2/classwx_sys_colour_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxSysColourChangedEventType` to subscribe to events of this type.

# `wxSysColourChanged`

```erlang
-type wxSysColourChanged() ::
          #wxSysColourChanged{type :: wxSysColourChangedEvent:wxSysColourChangedEventType()}.
```

# `wxSysColourChangedEvent`

```erlang
-type wxSysColourChangedEvent() :: wx:wx_object().
```

# `wxSysColourChangedEventType`

```erlang
-type wxSysColourChangedEventType() :: sys_colour_changed.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
