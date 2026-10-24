# `wxInitDialogEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxInitDialogEvent.erl#L58)

A `m:wxInitDialogEvent` is sent as a dialog or panel is being initialised.

Handlers for this event can transfer data to the window.

The default handler calls `wxWindow:transferDataToWindow/1`.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxInitDialogEvent](https://docs.wxwidgets.org/3.2/classwx_init_dialog_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxInitDialogEventType` to subscribe to events of this type.

# `wxInitDialog`

```erlang
-type wxInitDialog() :: #wxInitDialog{type :: wxInitDialogEvent:wxInitDialogEventType()}.
```

# `wxInitDialogEvent`

```erlang
-type wxInitDialogEvent() :: wx:wx_object().
```

# `wxInitDialogEventType`

```erlang
-type wxInitDialogEventType() :: init_dialog.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
