# `wxBitmapDataObject`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxBitmapDataObject.erl#L58)

`m:wxBitmapDataObject` is a specialization of `m:wxDataObject` for bitmap data.

It can be used without change to paste data into the `m:wxClipboard` or a `wxDropSource`
(not implemented in wx). A user may wish to derive a new class from this class for
providing a bitmap on-demand in order to minimize memory consumption when offering data in
several formats, such as a bitmap and GIF.

This class may be used as is, but `getBitmap/1` may be overridden to increase efficiency.

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* `m:wxDataObject`

* `m:wxFileDataObject`

* `m:wxTextDataObject`

* `m:wxDataObject`

This class is derived, and can use functions, from:

* `m:wxDataObject`

wxWidgets docs: [wxBitmapDataObject](https://docs.wxwidgets.org/3.2/classwx_bitmap_data_object.html)

# `wxBitmapDataObject`

```erlang
-type wxBitmapDataObject() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxBitmapDataObject()) -> ok.
```

Destroys the object

# `getBitmap`

```erlang
-spec getBitmap(This) -> wxBitmap:wxBitmap() when This :: wxBitmapDataObject().
```

Returns the bitmap associated with the data object.

You may wish to override this method when offering data on-demand, but this is not
required by wxWidgets' internals. Use this method to get data in bitmap form from the `m:wxClipboard`.

# `new`

```erlang
-spec new() -> wxBitmapDataObject().
```

# `new`

```erlang
-spec new([Option]) -> wxBitmapDataObject() when Option :: {bitmap, wxBitmap:wxBitmap()};
         (Bitmap) -> wxBitmapDataObject() when Bitmap :: wxBitmap:wxBitmap().
```

Constructor, optionally passing a bitmap (otherwise use `setBitmap/2` later).

# `setBitmap`

```erlang
-spec setBitmap(This, Bitmap) -> ok when This :: wxBitmapDataObject(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the bitmap associated with the data object.

This method is called when the data object receives data. Usually there will be no reason
to override this function.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
