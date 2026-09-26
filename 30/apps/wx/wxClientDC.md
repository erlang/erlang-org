# `wxClientDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxClientDC.erl#L58)

`m:wxClientDC` is primarily useful for obtaining information about the window from
outside EVT\_PAINT() handler.

Typical use of this class is to obtain the extent of some text string in order to
allocate enough size for a window, e.g.

Note: While `m:wxClientDC` may also be used for drawing on the client area of a window
from outside an EVT_PAINT() handler in some ports, this does `not` work on all platforms
(neither wxOSX nor wxGTK with GTK 3 Wayland backend support this, so drawing using `m:wxClientDC`
simply doesn't have any effect there) and the only portable way of drawing is via `m:wxPaintDC`.
To redraw a small part of the window, use `wxWindow:refreshRect/3` to invalidate just this part and check `wxWindow:getUpdateRegion/1` in the
paint event handler to redraw this part only.

`m:wxClientDC` objects should normally be constructed as temporary stack objects, i.e.
don't store a `m:wxClientDC` object.

A `m:wxClientDC` object is initialized to use the same font and colours as the window it
is associated with.

See:
* `m:wxDC`

* `m:wxMemoryDC`

* `m:wxPaintDC`

* `m:wxWindowDC`

* `m:wxScreenDC`

This class is derived, and can use functions, from:

* `m:wxWindowDC`

* `m:wxDC`

wxWidgets docs: [wxClientDC](https://docs.wxwidgets.org/3.2/classwx_client_d_c.html)

# `wxClientDC`

```erlang
-type wxClientDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxClientDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Window) -> wxClientDC() when Window :: wxWindow:wxWindow().
```

Constructor.

Pass a pointer to the window on which you wish to paint.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
