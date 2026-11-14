# `wxCursor`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCursor.erl#L58)

A cursor is a small bitmap usually used for denoting where the mouse pointer is, with a
picture that might indicate the interpretation of a mouse click.

As with icons, cursors in X and MS Windows are created in a different manner. Therefore,
separate cursors will be created for the different environments. Platform-specific methods
for creating a `m:wxCursor` object are catered for, and this is an occasion where
conditional compilation will probably be required (see `m:wxIcon` for an example).

A single cursor object may be used in many windows (any subwindow type). The wxWidgets
convention is to set the cursor for a window, as in X, rather than to set it globally as
in MS Windows, although a global `wx_misc:setCursor/1` function is also available for MS Windows use.

Creating a Custom Cursor

The following is an example of creating a cursor from 32x32 bitmap data (down_bits) and a
mask (down_mask) where 1 is black and 0 is white for the bits, and 1 is opaque and 0 is
transparent for the mask. It works on Windows and GTK+.

Predefined objects (include wx.hrl):

* ?wxNullCursor

* ?wxSTANDARD\_CURSOR

* ?wxHOURGLASS\_CURSOR

* ?wxCROSS\_CURSOR

See:
* `m:wxBitmap`

* `m:wxIcon`

* `wxWindow:setCursor/2`

* `wx_misc:setCursor/1`

* ?wxStockCursor

This class is derived, and can use functions, from:

* `m:wxBitmap`

wxWidgets docs: [wxCursor](https://docs.wxwidgets.org/3.2/classwx_cursor.html)

# `wxCursor`

```erlang
-type wxCursor() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxCursor()) -> ok.
```

Destroys the object

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxCursor().
```

Returns true if cursor data is present.

# `new`

```erlang
-spec new() -> wxCursor().
```

Default constructor.

# `new`

```erlang
-spec new(CursorName) -> wxCursor() when CursorName :: unicode:chardata();
         (Image) -> wxCursor() when Image :: wxImage:wxImage() | wxCursor:wxCursor();
         (CursorId) -> wxCursor() when CursorId :: wx:wx_enum().
```

Constructs a cursor using a cursor identifier.

# `new`

```erlang
-spec new(CursorName, [Option]) -> wxCursor()
             when
                 CursorName :: unicode:chardata(),
                 Option :: {type, wx:wx_enum()} | {hotSpotX, integer()} | {hotSpotY, integer()}.
```

Constructs a cursor by passing a string resource name or filename.

The arguments `hotSpotX` and `hotSpotY` are only used when there's no hotspot info in the
resource/image-file to load (e.g. when using `wxBITMAP_TYPE_ICO` under wxMSW or `wxBITMAP_TYPE_XPM`
under wxGTK).

# `ok`

```erlang
-spec ok(This) -> boolean() when This :: wxCursor().
```

Equivalent to: `isOk/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
