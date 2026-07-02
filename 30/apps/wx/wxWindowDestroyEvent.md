# `wxWindowDestroyEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxWindowDestroyEvent.erl#L58)

This event is sent as early as possible during the window destruction process.

For the top level windows, as early as possible means that this is done by `m:wxFrame` or `m:wxDialog`
destructor, i.e. after the destructor of the derived class was executed and so any
methods specific to the derived class can't be called any more from this event handler. If
you need to do this, you must call `wxWindow::SendDestroyEvent()` (not implemented in wx)
from your derived class destructor.

For the child windows, this event is generated just before deleting the window from `wxWindow:'Destroy'/1`
(which is also called when the parent window is deleted) or from the window destructor if
operator `delete` was used directly (which is not recommended for this very reason).

It is usually pointless to handle this event in the window itself but it ca be very
useful to receive notifications about the window destruction in the parent window or in
any other object interested in this window.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxWindowCreateEvent`

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxWindowDestroyEvent](https://docs.wxwidgets.org/3.2/classwx_window_destroy_event.html)

# `wxWindowDestroy`

```erlang
-type wxWindowDestroy() :: #wxWindowDestroy{type :: wxWindowDestroyEvent:wxWindowDestroyEventType()}.
```

# `wxWindowDestroyEvent`

```erlang
-type wxWindowDestroyEvent() :: wx:wx_object().
```

# `wxWindowDestroyEventType`

```erlang
-type wxWindowDestroyEventType() :: destroy.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
