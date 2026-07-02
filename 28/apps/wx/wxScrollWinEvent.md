# `wxScrollWinEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxScrollWinEvent.erl#L58)

A scroll event holds information about events sent from scrolling windows.

Note that you can use the EVT_SCROLLWIN* macros for intercepting scroll window events
from the receiving window.

See:
* `m:wxScrollEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxScrollWinEvent](https://docs.wxwidgets.org/3.2/classwx_scroll_win_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxScrollWinEventType` to subscribe to events of this type.

# `wxScrollWin`

```elixir
-type wxScrollWin() ::
          #wxScrollWin{type :: wxScrollWinEvent:wxScrollWinEventType(),
                       commandInt :: integer(),
                       extraLong :: integer()}.
```

# `wxScrollWinEvent`

```elixir
-type wxScrollWinEvent() :: wx:wx_object().
```

# `wxScrollWinEventType`

```elixir
-type wxScrollWinEventType() ::
          scrollwin_top | scrollwin_bottom | scrollwin_lineup | scrollwin_linedown | scrollwin_pageup |
          scrollwin_pagedown | scrollwin_thumbtrack | scrollwin_thumbrelease.
```

# `getOrientation`

```elixir
-spec getOrientation(This) -> integer() when This :: wxScrollWinEvent().
```

Returns wxHORIZONTAL or wxVERTICAL, depending on the orientation of the scrollbar.

# `getPosition`

```elixir
-spec getPosition(This) -> integer() when This :: wxScrollWinEvent().
```

Returns the position of the scrollbar for the thumb track and release events.

Note that this field can't be used for the other events, you need to query the window
itself for the current position in that case.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
