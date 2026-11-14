# `wxPaintEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPaintEvent.erl#L58)

A paint event is sent when a window's contents needs to be repainted.

The handler of this event must create a `m:wxPaintDC` object and use it for painting the
window contents. For example:

Notice that you must `not` create other kinds of `m:wxDC` (e.g. `m:wxClientDC` or `m:wxWindowDC`)
in EVT_PAINT handlers and also don't create `m:wxPaintDC` outside of this event handlers.

You can optimize painting by retrieving the rectangles that have been damaged and only
repainting these. The rectangles are in terms of the client area, and are unscrolled, so
you will need to do some calculations using the current view position to obtain logical,
scrolled units. Here is an example of using the `wxRegionIterator` (not implemented in wx)
class:

Remark: Please notice that in general it is impossible to change the drawing of a
standard control (such as `m:wxButton`) and so you shouldn't attempt to handle paint
events for them as even if it might work on some platforms, this is inherently not
portable and won't work everywhere.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxPaintEvent](https://docs.wxwidgets.org/3.2/classwx_paint_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxPaintEventType` to subscribe to events of this type.

# `wxPaint`

```erlang
-type wxPaint() :: #wxPaint{type :: wxPaintEvent:wxPaintEventType()}.
```

# `wxPaintEvent`

```erlang
-type wxPaintEvent() :: wx:wx_object().
```

# `wxPaintEventType`

```erlang
-type wxPaintEventType() :: paint.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
