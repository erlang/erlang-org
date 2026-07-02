# `wxMask`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMask.erl#L58)

This class encapsulates a monochrome mask bitmap, where the masked area is black and the
unmasked area is white.

When associated with a bitmap and drawn in a device context, the unmasked area of the
bitmap will be drawn, and the masked area will not be drawn.

Note: A mask can be associated also with a bitmap with an alpha channel but drawing such
bitmaps under wxMSW may be slow so using them should be avoided if drawing performance is
an important factor.

See:
* `m:wxBitmap`

* `wxDC:blit/6`

* `m:wxMemoryDC`

wxWidgets docs: [wxMask](https://docs.wxwidgets.org/3.2/classwx_mask.html)

# `wxMask`

```elixir
-type wxMask() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Bitmap) -> boolean() when This :: wxMask(), Bitmap :: wxBitmap:wxBitmap().
```

Constructs a mask from a monochrome bitmap.

# `create`

```elixir
-spec create(This, Bitmap, Index) -> boolean()
                when This :: wxMask(), Bitmap :: wxBitmap:wxBitmap(), Index :: integer();
            (This, Bitmap, Colour) -> boolean()
                when This :: wxMask(), Bitmap :: wxBitmap:wxBitmap(), Colour :: wx:wx_colour().
```

Constructs a mask from a bitmap and a colour that indicates the background.

# `destroy`

```elixir
-spec destroy(This :: wxMask()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new() -> wxMask().
```

Default constructor.

# `new`

```elixir
-spec new(Bitmap) -> wxMask() when Bitmap :: wxBitmap:wxBitmap().
```

Constructs a mask from a monochrome bitmap.

# `new`

```elixir
-spec new(Bitmap, Index) -> wxMask() when Bitmap :: wxBitmap:wxBitmap(), Index :: integer();
         (Bitmap, Colour) -> wxMask() when Bitmap :: wxBitmap:wxBitmap(), Colour :: wx:wx_colour().
```

Constructs a mask from a bitmap and a colour that indicates the background.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
