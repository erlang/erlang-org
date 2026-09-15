# `wxIcon`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxIcon.erl#L58)

An icon is a small rectangular bitmap usually used for denoting a minimized application.

It differs from a `m:wxBitmap` in always having a mask associated with it for transparent
drawing. On some platforms, icons and bitmaps are implemented identically, since there is
no real distinction between a `m:wxBitmap` with a mask and an icon; and there is no
specific icon format on some platforms (X-based applications usually standardize on XPMs
for small bitmaps and icons). However, some platforms (such as Windows) make the
distinction, so a separate class is provided.

Remark: It is usually desirable to associate a pertinent icon with a frame. Icons can
also be used for other purposes, for example with `m:wxTreeCtrl` and `m:wxListCtrl`. Icons
have different formats on different platforms therefore separate icons will usually be
created for the different environments. Platform-specific methods for creating a `m:wxIcon`
structure are catered for, and this is an occasion where conditional compilation will
probably be required. Note that a new icon must be created for every time the icon is to
be used for a new window. In Windows, the icon will not be reloaded if it has already been
used. An icon allocated to a frame will be deleted when the frame is deleted. For more
information please see overview_bitmap.

Predefined objects (include wx.hrl): ?wxNullIcon

See:
* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap)

* [Overview bitmap](https://docs.wxwidgets.org/3.2/overview_bitmap.html#overview_bitmap_supportedformats)

* `m:wxIconBundle`

* `wxDC:drawIcon/3`

* `m:wxCursor`

This class is derived, and can use functions, from:

* `m:wxBitmap`

wxWidgets docs: [wxIcon](https://docs.wxwidgets.org/3.2/classwx_icon.html)

# `wxIcon`

```erlang
-type wxIcon() :: wx:wx_object().
```

# `copyFromBitmap`

```erlang
-spec copyFromBitmap(This, Bmp) -> ok when This :: wxIcon(), Bmp :: wxBitmap:wxBitmap().
```

Copies `bmp` bitmap to this icon.

Under MS Windows the bitmap must have mask colour set.

See: `wxBitmap:loadFile/3`

# `destroy`

```erlang
-spec destroy(This :: wxIcon()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxIcon().
```

Default ctor.

Constructs an icon object with no data; an assignment or another member function such as `wxBitmap:loadFile/3`
must be called subsequently.

# `new`

```erlang
-spec new(Name) -> wxIcon() when Name :: unicode:chardata();
         (Icon) -> wxIcon() when Icon :: wxIcon().
```

Copy ctor.

# `new`

```erlang
-spec new(Name, [Option]) -> wxIcon()
             when
                 Name :: unicode:chardata(),
                 Option :: {type, wx:wx_enum()} | {desiredWidth, integer()} | {desiredHeight, integer()}.
```

Loads an icon from a file or resource.

See: `wxBitmap:loadFile/3`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
