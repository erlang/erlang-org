# `wxImageList`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxImageList.erl#L58)

A `m:wxImageList` contains a list of images, which are stored in an unspecified form.

Images can have masks for transparent drawing, and can be made from a variety of sources
including bitmaps and icons.

`m:wxImageList` is used principally in conjunction with `m:wxTreeCtrl` and `m:wxListCtrl` classes.

See:
* `m:wxTreeCtrl`

* `m:wxListCtrl`

wxWidgets docs: [wxImageList](https://docs.wxwidgets.org/3.2/classwx_image_list.html)

# `wxImageList`

```erlang
-type wxImageList() :: wx:wx_object().
```

# `add`

```erlang
-spec add(This, Icon) -> integer()
             when This :: wxImageList(), Icon :: wxIcon:wxIcon() | wxBitmap:wxBitmap().
```

Adds a new image using an icon.

Return: The new zero-based image index.

Remark: The original bitmap or icon is not affected by the `add/3` operation, and can be deleted
afterwards. If the bitmap is wider than the images in the list, then the bitmap will
automatically be split into smaller images, each matching the dimensions of the image
list. This does not apply when adding icons.

Only for:wxmsw,wxosx

# `add`

```erlang
-spec add(This, Bitmap, Mask) -> integer()
             when This :: wxImageList(), Bitmap :: wxBitmap:wxBitmap(), Mask :: wxBitmap:wxBitmap();
         (This, Bitmap, MaskColour) -> integer()
             when This :: wxImageList(), Bitmap :: wxBitmap:wxBitmap(), MaskColour :: wx:wx_colour().
```

Adds a new image or images using a bitmap and mask colour.

Return: The new zero-based image index.

Remark: The original bitmap or icon is not affected by the `add/3` operation, and can be deleted
afterwards. If the bitmap is wider than the images in the list, then the bitmap will
automatically be split into smaller images, each matching the dimensions of the image
list. This does not apply when adding icons.

# `create`

```erlang
-spec create(This, Width, Height) -> boolean()
                when This :: wxImageList(), Width :: integer(), Height :: integer().
```

# `create`

```erlang
-spec create(This, Width, Height, [Option]) -> boolean()
                when
                    This :: wxImageList(),
                    Width :: integer(),
                    Height :: integer(),
                    Option :: {mask, boolean()} | {initialCount, integer()}.
```

Initializes the list.

See `new/3` for details.

# `destroy`

```erlang
-spec destroy(This :: wxImageList()) -> ok.
```

Destroys the object

# `draw`

```erlang
-spec draw(This, Index, Dc, X, Y) -> boolean()
              when
                  This :: wxImageList(),
                  Index :: integer(),
                  Dc :: wxDC:wxDC(),
                  X :: integer(),
                  Y :: integer().
```

# `draw`

```erlang
-spec draw(This, Index, Dc, X, Y, [Option]) -> boolean()
              when
                  This :: wxImageList(),
                  Index :: integer(),
                  Dc :: wxDC:wxDC(),
                  X :: integer(),
                  Y :: integer(),
                  Option :: {flags, integer()} | {solidBackground, boolean()}.
```

Draws a specified image onto a device context.

# `getBitmap`

```erlang
-spec getBitmap(This, Index) -> wxBitmap:wxBitmap() when This :: wxImageList(), Index :: integer().
```

Returns the bitmap corresponding to the given index.

# `getIcon`

```erlang
-spec getIcon(This, Index) -> wxIcon:wxIcon() when This :: wxImageList(), Index :: integer().
```

Returns the icon corresponding to the given index.

# `getImageCount`

```erlang
-spec getImageCount(This) -> integer() when This :: wxImageList().
```

Returns the number of images in the list.

# `getSize`

```erlang
-spec getSize(This, Index) -> Result
                 when
                     Result :: {Res :: boolean(), Width :: integer(), Height :: integer()},
                     This :: wxImageList(),
                     Index :: integer().
```

Retrieves the size of the images in the list.

Currently, the `index` parameter is ignored as all images in the list have the same size.

Return: true if the function succeeded, false if it failed (for example, if the image
list was not yet initialized).

# `new`

```erlang
-spec new() -> wxImageList().
```

Default ctor.

# `new`

```erlang
-spec new(Width, Height) -> wxImageList() when Width :: integer(), Height :: integer().
```

# `new`

```erlang
-spec new(Width, Height, [Option]) -> wxImageList()
             when
                 Width :: integer(),
                 Height :: integer(),
                 Option :: {mask, boolean()} | {initialCount, integer()}.
```

Constructor specifying the image size, whether image masks should be created, and the
initial size of the list.

See: `create/4`

# `remove`

```erlang
-spec remove(This, Index) -> boolean() when This :: wxImageList(), Index :: integer().
```

Removes the image at the given position.

# `removeAll`

```erlang
-spec removeAll(This) -> boolean() when This :: wxImageList().
```

Removes all the images in the list.

# `replace`

```erlang
-spec replace(This, Index, Icon) -> boolean()
                 when
                     This :: wxImageList(),
                     Index :: integer(),
                     Icon :: wxIcon:wxIcon() | wxBitmap:wxBitmap().
```

Replaces the existing image with the new image.

Return: true if the replacement was successful, false otherwise.

Remark: The original bitmap or icon is not affected by the `replace/4` operation, and can be deleted afterwards.

Only for:wxmsw,wxosx

# `replace`

```erlang
-spec replace(This, Index, Bitmap, Mask) -> boolean()
                 when
                     This :: wxImageList(),
                     Index :: integer(),
                     Bitmap :: wxBitmap:wxBitmap(),
                     Mask :: wxBitmap:wxBitmap().
```

Replaces the existing image with the new image.

Windows only.

Return: true if the replacement was successful, false otherwise.

Remark: The original bitmap or icon is not affected by the `replace/4` operation, and can be deleted
afterwards.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
