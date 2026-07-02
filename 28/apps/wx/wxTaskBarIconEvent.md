# `wxTaskBarIconEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxTaskBarIconEvent.erl#L58)

The event class used by `m:wxTaskBarIcon`.

For a list of the event macros meant to be used with `m:wxTaskBarIconEvent`, please look
at `m:wxTaskBarIcon` description.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxTaskBarIconEvent](https://docs.wxwidgets.org/3.2/classwx_task_bar_icon_event.html)

# `wxTaskBarIcon`

```elixir
-type wxTaskBarIcon() :: #wxTaskBarIcon{type :: wxTaskBarIconEvent:wxTaskBarIconEventType()}.
```

# `wxTaskBarIconEvent`

```elixir
-type wxTaskBarIconEvent() :: wx:wx_object().
```

# `wxTaskBarIconEventType`

```elixir
-type wxTaskBarIconEventType() ::
          taskbar_move | taskbar_left_down | taskbar_left_up | taskbar_right_down | taskbar_right_up |
          taskbar_left_dclick | taskbar_right_dclick.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
