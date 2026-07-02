# `wxBufferedDC`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxBufferedDC.erl#L58)

This class provides a simple way to avoid flicker: when drawing on it, everything is in
fact first drawn on an in-memory buffer (a `m:wxBitmap`) and then copied to the screen,
using the associated `m:wxDC`, only once, when this object is destroyed.

`m:wxBufferedDC` itself is typically associated with `m:wxClientDC`, if you want to use
it in your `EVT_PAINT` handler, you should look at `m:wxBufferedPaintDC` instead.

When used like this, a valid `DC` must be specified in the constructor while the `buffer`
bitmap doesn't have to be explicitly provided, by default this class will allocate the
bitmap of required size itself. However using a dedicated bitmap can speed up the
redrawing process by eliminating the repeated creation and destruction of a possibly big
bitmap. Otherwise, `m:wxBufferedDC` can be used in the same way as any other device context.

Another possible use for `m:wxBufferedDC` is to use it to maintain a backing store for
the window contents. In this case, the associated `DC` may be NULL but a valid backing
store bitmap should be specified.

Finally, please note that GTK+ 2.0 as well as macOS provide double buffering themselves
natively. You can either use `wxWindow:isDoubleBuffered/1` to determine whether you need to use buffering or not, or
use `wxAutoBufferedPaintDC` (not implemented in wx) to avoid needless double buffering on
the systems which already do it automatically.

See:
* `m:wxDC`

* `m:wxMemoryDC`

* `m:wxBufferedPaintDC`

This class is derived, and can use functions, from:

* `m:wxMemoryDC`

* `m:wxDC`

wxWidgets docs: [wxBufferedDC](https://docs.wxwidgets.org/3.2/classwx_buffered_d_c.html)

# `wxBufferedDC`

```erlang
-type wxBufferedDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxBufferedDC()) -> ok.
```

Destroys the object

# `init`

```erlang
-spec init(This, Dc) -> ok when This :: wxBufferedDC(), Dc :: wxDC:wxDC().
```

# `init`

```erlang
-spec init(This, Dc, Area) -> ok
              when This :: wxBufferedDC(), Dc :: wxDC:wxDC(), Area :: {W :: integer(), H :: integer()};
          (This, Dc, [Option]) -> ok
              when
                  This :: wxBufferedDC(),
                  Dc :: wxDC:wxDC(),
                  Option :: {buffer, wxBitmap:wxBitmap()} | {style, integer()}.
```

# `init`

```erlang
-spec init(This, Dc, Area, [Option]) -> ok
              when
                  This :: wxBufferedDC(),
                  Dc :: wxDC:wxDC(),
                  Area :: {W :: integer(), H :: integer()},
                  Option :: {style, integer()}.
```

Initializes the object created using the default constructor.

Please see the constructors for parameter details.

# `new`

```erlang
-spec new() -> wxBufferedDC().
```

Default constructor.

You must call one of the `init/4` methods later in order to use the device context.

# `new`

```erlang
-spec new(Dc) -> wxBufferedDC() when Dc :: wxDC:wxDC().
```

# `new`

```erlang
-spec new(Dc, Area) -> wxBufferedDC() when Dc :: wxDC:wxDC(), Area :: {W :: integer(), H :: integer()};
         (Dc, [Option]) -> wxBufferedDC()
             when Dc :: wxDC:wxDC(), Option :: {buffer, wxBitmap:wxBitmap()} | {style, integer()}.
```

Creates a buffer for the provided dc.

`init/4` must not be called when using this constructor.

# `new`

```erlang
-spec new(Dc, Area, [Option]) -> wxBufferedDC()
             when
                 Dc :: wxDC:wxDC(),
                 Area :: {W :: integer(), H :: integer()},
                 Option :: {style, integer()}.
```

Creates a buffer for the provided `dc`.

`init/4` must not be called when using this constructor.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
