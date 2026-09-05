# `wxMouseCaptureChangedEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMouseCaptureChangedEvent.erl#L58)

An mouse capture changed event is sent to a window that loses its mouse capture.

This is called even if `wxWindow:releaseMouse/1` was called by the application code. Handling this event allows an
application to cater for unexpected capture releases which might otherwise confuse mouse
handling code.

Only for:wxmsw

See:
* `m:wxMouseCaptureLostEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:captureMouse/1`

* `wxWindow:releaseMouse/1`

* `wxWindow:getCapture/0`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMouseCaptureChangedEvent](https://docs.wxwidgets.org/3.2/classwx_mouse_capture_changed_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMouseCaptureChangedEventType` to subscribe to events of this type.

# `wxMouseCaptureChanged`

```erlang
-type wxMouseCaptureChanged() ::
          #wxMouseCaptureChanged{type :: wxMouseCaptureChangedEvent:wxMouseCaptureChangedEventType()}.
```

# `wxMouseCaptureChangedEvent`

```erlang
-type wxMouseCaptureChangedEvent() :: wx:wx_object().
```

# `wxMouseCaptureChangedEventType`

```erlang
-type wxMouseCaptureChangedEventType() :: mouse_capture_changed.
```

# `getCapturedWindow`

```erlang
-spec getCapturedWindow(This) -> wxWindow:wxWindow() when This :: wxMouseCaptureChangedEvent().
```

Returns the window that gained the capture, or NULL if it was a non-wxWidgets window.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
