# `wxScreenDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxScreenDC.erl#L58)

A `m:wxScreenDC` can be used to paint on the screen.

This should normally be constructed as a temporary stack object; don't store a `m:wxScreenDC`
object.

When using multiple monitors, `m:wxScreenDC` corresponds to the entire virtual screen
composed of all of them. Notice that coordinates on `m:wxScreenDC` can be negative in this
case, see `wxDisplay:getGeometry/1` for more.

See:
* `m:wxDC`

* `m:wxMemoryDC`

* `m:wxPaintDC`

* `m:wxClientDC`

* `m:wxWindowDC`

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxScreenDC](https://docs.wxwidgets.org/3.2/classwx_screen_d_c.html)

# `wxScreenDC`

```erlang
-type wxScreenDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxScreenDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxScreenDC().
```

Constructor.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
