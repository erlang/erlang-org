# `wxBrush`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxBrush.erl#L58)

A brush is a drawing tool for filling in areas.

It is used for painting the background of rectangles, ellipses, etc. It has a colour and
a style.

On a monochrome display, wxWidgets shows all brushes as white unless the colour is really black.

Do not initialize objects on the stack before the program commences, since other required
structures may not have been set up yet. Instead, define global pointers to objects and
create them in `wxApp::OnInit` (not implemented in wx) or when required.

An application may wish to create brushes with different characteristics dynamically, and
there is the consequent danger that a large number of duplicate brushes will be created.
Therefore an application may wish to get a pointer to a brush by using the global list of
brushes ?wxTheBrushList, and calling the member function `wxBrushList::FindOrCreateBrush()`
(not implemented in wx).

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

Predefined objects (include wx.hrl):

* ?wxNullBrush

* ?wxBLACK\_BRUSH

* ?wxBLUE\_BRUSH

* ?wxCYAN\_BRUSH

* ?wxGREEN\_BRUSH

* ?wxYELLOW\_BRUSH

* ?wxGREY\_BRUSH

* ?wxLIGHT\_GREY\_BRUSH

* ?wxMEDIUM\_GREY\_BRUSH

* ?wxRED\_BRUSH

* ?wxTRANSPARENT\_BRUSH

* ?wxWHITE\_BRUSH

See:
* `m:wxDC`

* `wxDC:setBrush/2`

wxWidgets docs: [wxBrush](https://docs.wxwidgets.org/3.2/classwx_brush.html)

# `wxBrush`

```erlang
-type wxBrush() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxBrush()) -> ok.
```

Destroys the object

# `getColour`

```erlang
-spec getColour(This) -> wx:wx_colour4() when This :: wxBrush().
```

Returns a reference to the brush colour.

See: `setColour/4`

# `getStipple`

```erlang
-spec getStipple(This) -> wxBitmap:wxBitmap() when This :: wxBrush().
```

Gets a pointer to the stipple bitmap.

If the brush does not have a `wxBRUSHSTYLE_STIPPLE` style, this bitmap may be non-NULL
but uninitialised (i.e. `m:wxBitmap`:`isOk/1` returns false).

See: `setStipple/2`

# `getStyle`

```erlang
-spec getStyle(This) -> wx:wx_enum() when This :: wxBrush().
```

Returns the brush style, one of the ?wxBrushStyle values.

See:
* `setStyle/2`

* `setColour/4`

* `setStipple/2`

# `isHatch`

```erlang
-spec isHatch(This) -> boolean() when This :: wxBrush().
```

Returns true if the style of the brush is any of hatched fills.

See: `getStyle/1`

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxBrush().
```

Returns true if the brush is initialised.

Notice that an uninitialized brush object can't be queried for any brush properties and
all calls to the accessor methods on it will result in an assert failure.

# `new`

```erlang
-spec new() -> wxBrush().
```

Default constructor.

The brush will be uninitialised, and `m:wxBrush`:`isOk/1` will return false.

# `new`

```erlang
-spec new(Colour) -> wxBrush() when Colour :: wx:wx_colour();
         (Brush) -> wxBrush() when Brush :: wxBrush:wxBrush() | wxBitmap:wxBitmap().
```

Copy constructor, uses reference counting.

# `new`

```erlang
-spec new(Colour, [Option]) -> wxBrush() when Colour :: wx:wx_colour(), Option :: {style, wx:wx_enum()}.
```

Constructs a brush from a colour object and `style`.

# `setColour`

```erlang
-spec setColour(This, Colour) -> ok when This :: wxBrush(), Colour :: wx:wx_colour().
```

Sets the brush colour using red, green and blue values.

See: `getColour/1`

# `setColour`

```erlang
-spec setColour(This, Red, Green, Blue) -> ok
                   when This :: wxBrush(), Red :: integer(), Green :: integer(), Blue :: integer().
```

# `setStipple`

```erlang
-spec setStipple(This, Bitmap) -> ok when This :: wxBrush(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the stipple bitmap.

Remark: The style will be set to `wxBRUSHSTYLE_STIPPLE`, unless the bitmap has a mask
associated to it, in which case the style will be set to `wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE`.

See: `m:wxBitmap`

# `setStyle`

```erlang
-spec setStyle(This, Style) -> ok when This :: wxBrush(), Style :: wx:wx_enum().
```

Sets the brush style.

See: `getStyle/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
