# `wxEraseEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxEraseEvent.erl#L58)

An erase event is sent when a window's background needs to be repainted.

On some platforms, such as GTK+, this event is simulated (simply generated just before
the paint event) and may cause flicker. It is therefore recommended that you set the text
background colour explicitly in order to prevent flicker. The default background colour
under GTK+ is grey.

To intercept this event, use the EVT_ERASE_BACKGROUND macro in an event table definition.

You must use the device context returned by `getDC/1` to draw on, don't create a `m:wxPaintDC` in
the event handler.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxEraseEvent](https://docs.wxwidgets.org/3.2/classwx_erase_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxEraseEventType` to subscribe to events of this type.

# `wxErase`

```erlang
-type wxErase() :: #wxErase{type :: wxEraseEvent:wxEraseEventType(), dc :: wxDC:wxDC()}.
```

# `wxEraseEvent`

```erlang
-type wxEraseEvent() :: wx:wx_object().
```

# `wxEraseEventType`

```erlang
-type wxEraseEventType() :: erase_background.
```

# `getDC`

```erlang
-spec getDC(This) -> wxDC:wxDC() when This :: wxEraseEvent().
```

Returns the device context associated with the erase event to draw on.

The returned pointer is never NULL.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
