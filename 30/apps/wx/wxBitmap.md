# `wxBitmap`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxBitmap.erl#L58)

This class encapsulates the concept of a platform-dependent bitmap, either monochrome or
colour or colour with alpha channel support.

If you need direct access the bitmap data instead going through drawing to it using `m:wxMemoryDC`
you need to use the `wxPixelData` (not implemented in wx) class (either wxNativePixelData
for RGB bitmaps or wxAlphaPixelData for bitmaps with an additionally alpha channel).

Note that many `m:wxBitmap` functions take a `type` parameter, which is a value of the
?wxBitmapType enumeration. The validity of those values depends however on the platform
where your program is running and from the wxWidgets configuration. If all possible
wxWidgets settings are used:

* wxMSW supports BMP and ICO files, BMP and ICO resources;

* wxGTK supports any file supported by gdk-pixbuf;

* wxMac supports PICT resources;

* wxX11 supports XPM files, XPM data, XBM data;

In addition, `m:wxBitmap` can load and save all formats that `m:wxImage` can; see `m:wxImage`
for more info. Of course, you must have loaded the `m:wxImage` handlers (see
?wxInitAllImageHandlers() and `wxImage::AddHandler` (not implemented in wx)). Note that
all available wxBitmapHandlers for a given wxWidgets port are automatically loaded at
startup so you won't need to use `wxBitmap::AddHandler` (not implemented in wx).

More on the difference between `m:wxImage` and `m:wxBitmap`: `m:wxImage` is just a buffer
of RGB bytes with an optional buffer for the alpha bytes. It is all generic, platform
independent and image file format independent code. It includes generic code for scaling,
resizing, clipping, and other manipulations of the image data. OTOH, `m:wxBitmap` is
intended to be a wrapper of whatever is the native image format that is quickest/easiest
to draw to a DC or to be the target of the drawing operations performed on a `m:wxMemoryDC`.
By splitting the responsibilities between wxImage/wxBitmap like this then it's easier to
use generic code shared by all platforms and image types for generic operations and
platform specific code where performance or compatibility is needed.

Predefined objects (include wx.hrl): ?wxNullBitmap

See:
* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap)

* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap_supportedformats)

* `wxDC:blit/6`

* `m:wxIcon`

* `m:wxCursor`

* `m:wxMemoryDC`

* `m:wxImage`

wxWidgets docs: [wxBitmap](https://docs.wxwidgets.org/3.2/classwx_bitmap.html)

# `wxBitmap`

```erlang
-type wxBitmap() :: wx:wx_object().
```

# `convertToImage`

```erlang
-spec convertToImage(This) -> wxImage:wxImage() when This :: wxBitmap().
```

Creates an image from a platform-dependent bitmap.

This preserves mask information so that bitmaps and images can be converted back and
forth without loss in that respect.

# `copyFromIcon`

```erlang
-spec copyFromIcon(This, Icon) -> boolean() when This :: wxBitmap(), Icon :: wxIcon:wxIcon().
```

Creates the bitmap from an icon.

# `create`

```erlang
-spec create(This, Sz) -> boolean() when This :: wxBitmap(), Sz :: {W :: integer(), H :: integer()}.
```

# `create`

```erlang
-spec create(This, Width, Height) -> boolean()
                when This :: wxBitmap(), Width :: integer(), Height :: integer();
            (This, Sz, [Option]) -> boolean()
                when
                    This :: wxBitmap(),
                    Sz :: {W :: integer(), H :: integer()},
                    Option :: {depth, integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `create`

```erlang
-spec create(This, Width, Height, [Option]) -> boolean()
                when
                    This :: wxBitmap(),
                    Width :: integer(),
                    Height :: integer(),
                    Option :: {depth, integer()};
            (This, Width, Height, Dc) -> boolean()
                when This :: wxBitmap(), Width :: integer(), Height :: integer(), Dc :: wxDC:wxDC().
```

Create a bitmap compatible with the given DC, inheriting its magnification factor.

Return: true if the creation was successful.

Since: 3.1.0

# `destroy`

```erlang
-spec destroy(This :: wxBitmap()) -> ok.
```

Destroys the object

# `getDepth`

```erlang
-spec getDepth(This) -> integer() when This :: wxBitmap().
```

Gets the colour depth of the bitmap.

A value of 1 indicates a monochrome bitmap.

# `getHeight`

```erlang
-spec getHeight(This) -> integer() when This :: wxBitmap().
```

Gets the height of the bitmap in pixels.

See: `getWidth/1`

# `getMask`

```erlang
-spec getMask(This) -> wxMask:wxMask() when This :: wxBitmap().
```

Gets the associated mask (if any) which may have been loaded from a file or set for the
bitmap.

See:
* `setMask/2`

* `m:wxMask`

# `getPalette`

```erlang
-spec getPalette(This) -> wxPalette:wxPalette() when This :: wxBitmap().
```

Gets the associated palette (if any) which may have been loaded from a file or set for
the bitmap.

See: `m:wxPalette`

# `getSubBitmap`

```erlang
-spec getSubBitmap(This, Rect) -> wxBitmap()
                      when
                          This :: wxBitmap(),
                          Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

Returns a sub bitmap of the current one as long as the rect belongs entirely to the
bitmap.

This function preserves bit depth and mask information.

# `getWidth`

```erlang
-spec getWidth(This) -> integer() when This :: wxBitmap().
```

Gets the width of the bitmap in pixels.

See: `getHeight/1`

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxBitmap().
```

Returns true if bitmap data is present.

# `loadFile`

```erlang
-spec loadFile(This, Name) -> boolean() when This :: wxBitmap(), Name :: unicode:chardata().
```

# `loadFile`

```erlang
-spec loadFile(This, Name, [Option]) -> boolean()
                  when This :: wxBitmap(), Name :: unicode:chardata(), Option :: {type, wx:wx_enum()}.
```

Loads a bitmap from a file or resource.

Return: true if the operation succeeded, false otherwise.

Remark: A palette may be associated with the bitmap if one exists (especially for colour
Windows bitmaps), and if the code supports it. You can check if one has been created by
using the `getPalette/1` member.

See: `saveFile/4`

# `new`

```erlang
-spec new() -> wxBitmap().
```

Default constructor.

Constructs a bitmap object with no data; an assignment or another member function such as `create/4`
or `loadFile/3` must be called subsequently.

# `new`

```erlang
-spec new(Name) -> wxBitmap() when Name :: unicode:chardata();
         (Sz) -> wxBitmap() when Sz :: {W :: integer(), H :: integer()};
         (Img) -> wxBitmap() when Img :: wxImage:wxImage() | wxBitmap:wxBitmap().
```

# `new`

```erlang
-spec new(Width, Height) -> wxBitmap() when Width :: integer(), Height :: integer();
         (Name, [Option]) -> wxBitmap() when Name :: unicode:chardata(), Option :: {type, wx:wx_enum()};
         (Sz, [Option]) -> wxBitmap()
             when Sz :: {W :: integer(), H :: integer()}, Option :: {depth, integer()};
         (Img, [Option]) -> wxBitmap() when Img :: wxImage:wxImage(), Option :: {depth, integer()}.
```

Creates this bitmap object from the given image.

This has to be done to actually display an image as you cannot draw an image directly on
a window.

The resulting bitmap will use the provided colour depth (or that of the current system if
depth is ?wxBITMAP\_SCREEN\_DEPTH) which entails that a colour reduction may take place.

On Windows, if there is a palette present (set with SetPalette), it will be used when
creating the `m:wxBitmap` (most useful in 8-bit display mode). On other platforms, the
palette is currently ignored.

# `new`

```erlang
-spec new(Bits, Width, Height) -> wxBitmap()
             when Bits :: binary(), Width :: integer(), Height :: integer();
         (Width, Height, [Option]) -> wxBitmap()
             when Width :: integer(), Height :: integer(), Option :: {depth, integer()}.
```

Creates a new bitmap.

A depth of ?wxBITMAP\_SCREEN\_DEPTH indicates the depth of the current screen or visual.

Some platforms only support 1 for monochrome and ?wxBITMAP\_SCREEN\_DEPTH for the current
colour setting.

A depth of 32 including an alpha channel is supported under MSW, Mac and GTK+.

# `new`

```erlang
-spec new(Bits, Width, Height, [Option]) -> wxBitmap()
             when
                 Bits :: binary(), Width :: integer(), Height :: integer(), Option :: {depth, integer()}.
```

Creates a bitmap from the given array `bits`.

You should only use this function for monochrome bitmaps (depth 1) in portable programs:
in this case the bits parameter should contain an XBM image.

For other bit depths, the behaviour is platform dependent: under Windows, the data is
passed without any changes to the underlying CreateBitmap() API. Under other platforms,
only monochrome bitmaps may be created using this constructor and `m:wxImage` should be
used for creating colour bitmaps from static data.

# `ok`

```erlang
-spec ok(This) -> boolean() when This :: wxBitmap().
```

Equivalent to: `isOk/1`

# `saveFile`

```erlang
-spec saveFile(This, Name, Type) -> boolean()
                  when This :: wxBitmap(), Name :: unicode:chardata(), Type :: wx:wx_enum().
```

# `saveFile`

```erlang
-spec saveFile(This, Name, Type, [Option]) -> boolean()
                  when
                      This :: wxBitmap(),
                      Name :: unicode:chardata(),
                      Type :: wx:wx_enum(),
                      Option :: {palette, wxPalette:wxPalette()}.
```

Saves a bitmap in the named file.

Return: true if the operation succeeded, false otherwise.

Remark: Depending on how wxWidgets has been configured, not all formats may be available.

See: `loadFile/3`

# `setDepth`

```erlang
-spec setDepth(This, Depth) -> ok when This :: wxBitmap(), Depth :: integer().
```

Deprecated:

This function is deprecated since version 3.1.2, dimensions and depth can only be set at
construction time.

Sets the depth member (does not affect the bitmap data).

# `setHeight`

```erlang
-spec setHeight(This, Height) -> ok when This :: wxBitmap(), Height :: integer().
```

Deprecated:

This function is deprecated since version 3.1.2, dimensions and depth can only be set at
construction time.

Sets the height member (does not affect the bitmap data).

# `setMask`

```erlang
-spec setMask(This, Mask) -> ok when This :: wxBitmap(), Mask :: wxMask:wxMask().
```

Sets the mask for this bitmap.

Remark: The bitmap object owns the mask once this has been called.

Note: A mask can be set also for bitmap with an alpha channel but doing so under wxMSW is
not recommended because performance of drawing such bitmap is not very good.

See:
* `getMask/1`

* `m:wxMask`

# `setPalette`

```erlang
-spec setPalette(This, Palette) -> ok when This :: wxBitmap(), Palette :: wxPalette:wxPalette().
```

Sets the associated palette.

(Not implemented under GTK+).

See: `m:wxPalette`

# `setWidth`

```erlang
-spec setWidth(This, Width) -> ok when This :: wxBitmap(), Width :: integer().
```

Deprecated:

This function is deprecated since version 3.1.2, dimensions and depth can only be set at
construction time.

Sets the width member (does not affect the bitmap data).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
