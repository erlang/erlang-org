# `wxIdleEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxIdleEvent.erl#L58)

This class is used for idle events, which are generated when the system becomes idle.

Note that, unless you do something specifically, the idle events are not sent if the
system remains idle once it has become it, e.g. only a single idle event will be generated
until something else resulting in more normal events happens and only then is the next
idle event sent again.

If you need to ensure a continuous stream of idle events, you can either use `requestMore/2` method in
your handler or call ?wxWakeUpIdle() periodically (for example from a timer event
handler), but note that both of these approaches (and especially the first one) increase
the system load and so should be avoided if possible.

By default, idle events are sent to all windows, including even the hidden ones because
they may be shown if some condition is met from their `wxEVT_IDLE` (or related `wxEVT_UPDATE_UI`)
handler. The children of hidden windows do not receive idle events however as they can't
change their state in any way noticeable by the user. Finally, the global `wxApp` (not
implemented in wx) object also receives these events, as usual, so it can be used for any
global idle time processing.

If sending idle events to all windows is causing a significant overhead in your
application, you can call `setMode/1` with the value wxIDLE_PROCESS_SPECIFIED, and set the
wxWS_EX_PROCESS_IDLE extra window style for every window which should receive idle events,
all the other ones will not receive them in this case.

Delayed Action Mechanism

`m:wxIdleEvent` can be used to perform some action "at slightly later time". This can be
necessary in several circumstances when, for whatever reason, something can't be done in
the current event handler. For example, if a mouse event handler is called with the mouse
button pressed, the mouse can be currently captured and some operations with it - notably
capturing it again - might be impossible or lead to undesirable results. If you still want
to capture it, you can do it from `wxEVT_IDLE` handler when it is called the next time
instead of doing it immediately.

This can be achieved in two different ways: when using static event tables, you will need
a flag indicating to the (always connected) idle event handler whether the desired action
should be performed. The originally called handler would then set it to indicate that it
should indeed be done and the idle handler itself would reset it to prevent it from doing
the same action again.

Using dynamically connected event handlers things are even simpler as the original event
handler can simply `wxEvtHandler::Connect()` (not implemented in wx) or `wxEvtHandler::Bind()`
(not implemented in wx) the idle event handler which would only be executed then and
could `wxEvtHandler::Disconnect()` (not implemented in wx) or `wxEvtHandler::Unbind()`
(not implemented in wx) itself.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxUpdateUIEvent`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxIdleEvent](https://docs.wxwidgets.org/3.2/classwx_idle_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxIdleEventType` to subscribe to events of this type.

# `wxIdle`

```erlang
-type wxIdle() :: #wxIdle{type :: wxIdleEvent:wxIdleEventType()}.
```

# `wxIdleEvent`

```erlang
-type wxIdleEvent() :: wx:wx_object().
```

# `wxIdleEventType`

```erlang
-type wxIdleEventType() :: idle.
```

# `getMode`

```erlang
-spec getMode() -> wx:wx_enum().
```

Static function returning a value specifying how wxWidgets will send idle events: to all
windows, or only to those which specify that they will process the events.

See: `setMode/1`

# `moreRequested`

```erlang
-spec moreRequested(This) -> boolean() when This :: wxIdleEvent().
```

Returns true if the OnIdle function processing this event requested more processing time.

See: `requestMore/2`

# `requestMore`

```erlang
-spec requestMore(This) -> ok when This :: wxIdleEvent().
```

# `requestMore`

```erlang
-spec requestMore(This, [Option]) -> ok when This :: wxIdleEvent(), Option :: {needMore, boolean()}.
```

Tells wxWidgets that more processing is required.

This function can be called by an OnIdle handler for a window or window event handler to
indicate that wxApp::OnIdle should forward the OnIdle event once more to the application windows.

If no window calls this function during OnIdle, then the application will remain in a
passive event loop (not calling OnIdle) until a new event is posted to the application by
the windowing system.

See: `moreRequested/1`

# `setMode`

```erlang
-spec setMode(Mode) -> ok when Mode :: wx:wx_enum().
```

Static function for specifying how wxWidgets will send idle events: to all windows, or
only to those which specify that they will process the events.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
