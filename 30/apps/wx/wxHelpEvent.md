# `wxHelpEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxHelpEvent.erl#L58)

A help event is sent when the user has requested context-sensitive help.

This can either be caused by the application requesting context-sensitive help mode via `wxContextHelp`
(not implemented in wx), or (on MS Windows) by the system generating a WM_HELP message
when the user pressed F1 or clicked on the query button in a dialog caption.

A help event is sent to the window that the user clicked on, and is propagated up the
window hierarchy until the event is processed or there are no more event handlers.

The application should call `wxEvent:getId/1` to check the identity of the clicked-on window, and then
either show some suitable help or call `wxEvent:skip/2` if the identifier is unrecognised.

Calling Skip is important because it allows wxWidgets to generate further events for
ancestors of the clicked-on window. Otherwise it would be impossible to show help for
container windows, since processing would stop after the first window found.

See:
* `m:wxDialog`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxHelpEvent](https://docs.wxwidgets.org/3.2/classwx_help_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxHelpEventType` to subscribe to events of this type.

# `wxHelp`

```erlang
-type wxHelp() :: #wxHelp{type :: wxHelpEvent:wxHelpEventType()}.
```

# `wxHelpEvent`

```erlang
-type wxHelpEvent() :: wx:wx_object().
```

# `wxHelpEventType`

```erlang
-type wxHelpEventType() :: help | detailed_help.
```

# `getOrigin`

```erlang
-spec getOrigin(This) -> wx:wx_enum() when This :: wxHelpEvent().
```

Returns the origin of the help event which is one of the `wxHelpEvent::Origin` (not
implemented in wx) values.

The application may handle events generated using the keyboard or mouse differently, e.g.
by using `wx_misc:getMousePosition/0` for the mouse events.

See: `setOrigin/2`

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxHelpEvent().
```

Returns the left-click position of the mouse, in screen coordinates.

This allows the application to position the help appropriately.

# `setOrigin`

```erlang
-spec setOrigin(This, Origin) -> ok when This :: wxHelpEvent(), Origin :: wx:wx_enum().
```

Set the help event origin, only used internally by wxWidgets normally.

See: `getOrigin/1`

# `setPosition`

```erlang
-spec setPosition(This, Pt) -> ok when This :: wxHelpEvent(), Pt :: {X :: integer(), Y :: integer()}.
```

Sets the left-click position of the mouse, in screen coordinates.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
