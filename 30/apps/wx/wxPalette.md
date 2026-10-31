# `wxPalette`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPalette.erl#L58)

A palette is a table that maps pixel values to RGB colours.

It allows the colours of a low-depth bitmap, for example, to be mapped to the available
colours in a display. The notion of palettes is becoming more and more obsolete nowadays
and only the MSW port is still using a native palette. All other ports use generic code
which is basically just an array of colours.

It is likely that in the future the only use for palettes within wxWidgets will be for
representing colour indices from images (such as GIF or PNG). The image handlers for these
formats have been modified to create a palette if there is such information in the
original image file (usually 256 or less colour images). See `m:wxImage` for more information.

Predefined objects (include wx.hrl): ?wxNullPalette

See:
* `wxDC:setPalette/2`

* `m:wxBitmap`

wxWidgets docs: [wxPalette](https://docs.wxwidgets.org/3.2/classwx_palette.html)

# `wxPalette`

```erlang
-type wxPalette() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Red, Green, Blue) -> boolean()
                when This :: wxPalette(), Red :: binary(), Green :: binary(), Blue :: binary().
```

Creates a palette from arrays of size `n`, one for each red, blue or green component.

Return: true if the creation was successful, false otherwise.

See: `new/3`

# `destroy`

```erlang
-spec destroy(This :: wxPalette()) -> ok.
```

Destroys the object

# `getColoursCount`

```erlang
-spec getColoursCount(This) -> integer() when This :: wxPalette().
```

Returns number of entries in palette.

# `getPixel`

```erlang
-spec getPixel(This, Red, Green, Blue) -> integer()
                  when This :: wxPalette(), Red :: integer(), Green :: integer(), Blue :: integer().
```

Returns a pixel value (index into the palette) for the given RGB values.

Return: The nearest palette index or `wxNOT_FOUND` for unexpected errors.

See: `getRGB/2`

# `getRGB`

```erlang
-spec getRGB(This, Pixel) -> Result
                when
                    Result ::
                        {Res :: boolean(), Red :: integer(), Green :: integer(), Blue :: integer()},
                    This :: wxPalette(),
                    Pixel :: integer().
```

Returns RGB values for a given palette index.

Return: true if the operation was successful.

See: `getPixel/4`

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxPalette().
```

Returns true if palette data is present.

# `new`

```erlang
-spec new() -> wxPalette().
```

Default constructor.

# `new`

```erlang
-spec new(Palette) -> wxPalette() when Palette :: wxPalette().
```

Copy constructor, uses overview_refcount.

# `new`

```erlang
-spec new(Red, Green, Blue) -> wxPalette() when Red :: binary(), Green :: binary(), Blue :: binary().
```

Creates a palette from arrays of size `n`, one for each red, blue or green component.

See: `create/4`

# `ok`

```erlang
-spec ok(This) -> boolean() when This :: wxPalette().
```

Equivalent to: `isOk/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
