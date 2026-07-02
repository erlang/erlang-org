# `wxBufferedPaintDC`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxBufferedPaintDC.erl#L58)

This is a subclass of `m:wxBufferedDC` which can be used inside of an `EVT\_PAINT()`
event handler to achieve double-buffered drawing.

Just use this class instead of `m:wxPaintDC` and make sure `wxWindow:setBackgroundStyle/2` is called with
wxBG_STYLE_PAINT somewhere in the class initialization code, and that's all you have to do
to (mostly) avoid flicker. The only thing to watch out for is that if you are using this
class together with `wxScrolled` (not implemented in wx), you probably do `not` want to
call `wxScrolledWindow:prepareDC/2` on it as it already does this internally for the real underlying `m:wxPaintDC`.

See:
* `m:wxDC`

* `m:wxBufferedDC`

* `m:wxPaintDC`

This class is derived, and can use functions, from:

* `m:wxBufferedDC`

* `m:wxMemoryDC`

* `m:wxDC`

wxWidgets docs: [wxBufferedPaintDC](https://docs.wxwidgets.org/3.2/classwx_buffered_paint_d_c.html)

# `wxBufferedPaintDC`

```elixir
-type wxBufferedPaintDC() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxBufferedPaintDC()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new(Window) -> wxBufferedPaintDC() when Window :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Window, Buffer) -> wxBufferedPaintDC()
             when Window :: wxWindow:wxWindow(), Buffer :: wxBitmap:wxBitmap();
         (Window, [Option]) -> wxBufferedPaintDC()
             when Window :: wxWindow:wxWindow(), Option :: {style, integer()}.
```

# `new`

```elixir
-spec new(Window, Buffer, [Option]) -> wxBufferedPaintDC()
             when
                 Window :: wxWindow:wxWindow(),
                 Buffer :: wxBitmap:wxBitmap(),
                 Option :: {style, integer()}.
```

As with `m:wxBufferedDC`, you may either provide the bitmap to be used for buffering or
let this object create one internally (in the latter case, the size of the client part of
the window is used).

Pass wxBUFFER_CLIENT_AREA for the `style` parameter to indicate that just the client area
of the window is buffered, or wxBUFFER_VIRTUAL_AREA to indicate that the buffer bitmap
covers the virtual area.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
