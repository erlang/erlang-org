# `wxChildFocusEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxChildFocusEvent.erl#L58)

A child focus event is sent to a (parent-)window when one of its child windows gains
focus, so that the window could restore the focus back to its corresponding child if it
loses it now and regains later.

Notice that child window is the direct child of the window receiving event. Use `wxWindow:findFocus/0` to
retrieve the window which is actually getting focus.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxChildFocusEvent](https://docs.wxwidgets.org/3.2/classwx_child_focus_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxChildFocusEventType` to subscribe to events of this type.

# `wxChildFocus`

```elixir
-type wxChildFocus() :: #wxChildFocus{type :: wxChildFocusEvent:wxChildFocusEventType()}.
```

# `wxChildFocusEvent`

```elixir
-type wxChildFocusEvent() :: wx:wx_object().
```

# `wxChildFocusEventType`

```elixir
-type wxChildFocusEventType() :: child_focus.
```

# `getWindow`

```elixir
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxChildFocusEvent().
```

Returns the direct child which receives the focus, or a (grand-)parent of the control
receiving the focus.

To get the actually focused control use `wxWindow:findFocus/0`.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
