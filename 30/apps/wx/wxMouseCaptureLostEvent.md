# `wxMouseCaptureLostEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMouseCaptureLostEvent.erl#L58)

A mouse capture lost event is sent to a window that had obtained mouse capture, which was
subsequently lost due to an "external" event (for example, when a dialog box is shown or
if another application captures the mouse).

If this happens, this event is sent to all windows that are on the capture stack (i.e.
called CaptureMouse, but didn't call ReleaseMouse yet). The event is not sent if the
capture changes because of a call to CaptureMouse or ReleaseMouse.

This event is currently emitted under Windows only.

See:
* `m:wxMouseCaptureChangedEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `wxWindow:captureMouse/1`

* `wxWindow:releaseMouse/1`

* `wxWindow:getCapture/0`

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxMouseCaptureLostEvent](https://docs.wxwidgets.org/3.2/classwx_mouse_capture_lost_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxMouseCaptureLostEventType` to subscribe to events of this type.

# `wxMouseCaptureLost`

```erlang
-type wxMouseCaptureLost() ::
          #wxMouseCaptureLost{type :: wxMouseCaptureLostEvent:wxMouseCaptureLostEventType()}.
```

# `wxMouseCaptureLostEvent`

```erlang
-type wxMouseCaptureLostEvent() :: wx:wx_object().
```

# `wxMouseCaptureLostEventType`

```erlang
-type wxMouseCaptureLostEventType() :: mouse_capture_lost.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
