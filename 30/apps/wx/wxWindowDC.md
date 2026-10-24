# `wxWindowDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxWindowDC.erl#L58)

A `m:wxWindowDC` must be constructed if an application wishes to paint on the whole area
of a window (client and decorations).

This should normally be constructed as a temporary stack object; don't store a `m:wxWindowDC`
object.

To draw on a window from inside an EVT_PAINT() handler, construct a `m:wxPaintDC` object instead.

To draw on the client area of a window from outside an EVT_PAINT() handler, construct a `m:wxClientDC`
object.

A `m:wxWindowDC` object is initialized to use the same font and colours as the window it
is associated with.

See:
* `m:wxDC`

* `m:wxMemoryDC`

* `m:wxPaintDC`

* `m:wxClientDC`

* `m:wxScreenDC`

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxWindowDC](https://docs.wxwidgets.org/3.2/classwx_window_d_c.html)

# `wxWindowDC`

```erlang
-type wxWindowDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxWindowDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Window) -> wxWindowDC() when Window :: wxWindow:wxWindow().
```

Constructor.

Pass a pointer to the window on which you wish to paint.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
