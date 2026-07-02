# `wxScrollEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxScrollEvent.erl#L58)

A scroll event holds information about events sent from stand-alone scrollbars (see `m:wxScrollBar`)
and sliders (see `m:wxSlider`).

Note that scrolled windows send the `m:wxScrollWinEvent` which does not derive from `m:wxCommandEvent`,
but from `m:wxEvent` directly - don't confuse these two kinds of events and use the event
table macros mentioned below only for the scrollbar-like controls.

The difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED

The EVT_SCROLL_THUMBRELEASE event is only emitted when actually dragging the thumb using
the mouse and releasing it (This EVT_SCROLL_THUMBRELEASE event is also followed by an
EVT_SCROLL_CHANGED event).

The EVT_SCROLL_CHANGED event also occurs when using the keyboard to change the thumb
position, and when clicking next to the thumb (In all these cases the
EVT_SCROLL_THUMBRELEASE event does not happen).

In short, the EVT_SCROLL_CHANGED event is triggered when scrolling/ moving has finished
independently of the way it had started. Please see the page_samples_widgets ("Slider"
page) to see the difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED in action.

Remark: Note that unless specifying a scroll control identifier, you will need to test
for scrollbar orientation with `getOrientation/1`, since horizontal and vertical scroll events are processed
using the same event handler.

See:
* `m:wxScrollBar`

* `m:wxSlider`

* `m:wxSpinButton`

* `m:wxScrollWinEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxScrollEvent](https://docs.wxwidgets.org/3.2/classwx_scroll_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxScrollEventType` to subscribe to events of this type.

# `wxScroll`

```erlang
-type wxScroll() ::
          #wxScroll{type :: wxScrollEvent:wxScrollEventType(),
                    commandInt :: integer(),
                    extraLong :: integer()}.
```

# `wxScrollEvent`

```erlang
-type wxScrollEvent() :: wx:wx_object().
```

# `wxScrollEventType`

```erlang
-type wxScrollEventType() ::
          scroll_top | scroll_bottom | scroll_lineup | scroll_linedown | scroll_pageup |
          scroll_pagedown | scroll_thumbtrack | scroll_thumbrelease | scroll_changed.
```

# `getOrientation`

```erlang
-spec getOrientation(This) -> integer() when This :: wxScrollEvent().
```

Returns wxHORIZONTAL or wxVERTICAL, depending on the orientation of the scrollbar.

# `getPosition`

```erlang
-spec getPosition(This) -> integer() when This :: wxScrollEvent().
```

Returns the position of the scrollbar.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
