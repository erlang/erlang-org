# `wxFocusEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFocusEvent.erl#L58)

A focus event is sent when a window's focus changes.

The window losing focus receives a "kill focus" event while the window gaining it gets a
"set focus" one.

Notice that the set focus event happens both when the user gives focus to the window
(whether using the mouse or keyboard) and when it is done from the program itself using `wxWindow:setFocus/1`.

The focus event handlers should almost invariably call `wxEvent:skip/2` on their event argument to allow
the default handling to take place. Failure to do this may result in incorrect behaviour
of the native controls. Also note that wxEVT_KILL_FOCUS handler must not call `wxWindow:setFocus/1` as this,
again, is not supported by all native controls. If you need to do this, consider using the `Delayed Action Mechanism`
(not implemented in wx) described in `m:wxIdleEvent` documentation.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxFocusEvent](https://docs.wxwidgets.org/3.2/classwx_focus_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxFocusEventType` to subscribe to events of this type.

# `wxFocus`

```erlang
-type wxFocus() :: #wxFocus{type :: wxFocusEvent:wxFocusEventType(), win :: wxWindow:wxWindow()}.
```

# `wxFocusEvent`

```erlang
-type wxFocusEvent() :: wx:wx_object().
```

# `wxFocusEventType`

```erlang
-type wxFocusEventType() :: set_focus | kill_focus.
```

# `getWindow`

```erlang
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxFocusEvent().
```

Returns the window associated with this event, that is the window which had the focus
before for the `wxEVT\_SET\_FOCUS` event and the window which is going to receive focus
for the `wxEVT\_KILL\_FOCUS` one.

Warning: the window pointer may be NULL!

---

*Consult [api-reference.md](api-reference.md) for complete listing*
