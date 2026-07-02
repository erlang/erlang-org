# `wxPaintDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPaintDC.erl#L58)

A `m:wxPaintDC` must be constructed if an application wishes to paint on the client area
of a window from within an EVT\_PAINT() event handler.

This should normally be constructed as a temporary stack object; don't store a `m:wxPaintDC`
object. If you have an EVT_PAINT() handler, you `must` create a `m:wxPaintDC` object
within it even if you don't actually use it.

Using `m:wxPaintDC` within your EVT_PAINT() handler is important because it automatically
sets the clipping area to the damaged area of the window. Attempts to draw outside this
area do not appear.

A `m:wxPaintDC` object is initialized to use the same font and colours as the window it
is associated with.

See:
* `m:wxDC`

* `m:wxClientDC`

* `m:wxMemoryDC`

* `m:wxWindowDC`

* `m:wxScreenDC`

This class is derived, and can use functions, from:

* `m:wxWindowDC`

* `m:wxDC`

wxWidgets docs: [wxPaintDC](https://docs.wxwidgets.org/3.2/classwx_paint_d_c.html)

# `wxPaintDC`

```erlang
-type wxPaintDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPaintDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Window) -> wxPaintDC() when Window :: wxWindow:wxWindow().
```

Constructor.

Pass a pointer to the window on which you wish to paint.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
