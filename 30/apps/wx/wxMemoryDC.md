# `wxMemoryDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMemoryDC.erl#L58)

A memory device context provides a means to draw graphics onto a bitmap.

When drawing in to a mono-bitmap, using `wxWHITE`, `wxWHITE_PEN` and `wxWHITE_BRUSH` will
draw the background colour (i.e. 0) whereas all other colours will draw the foreground
colour (i.e. 1).

A bitmap must be selected into the new memory DC before it may be used for anything.
Typical usage is as follows:

Note that the memory DC must be deleted (or the bitmap selected out of it) before a
bitmap can be reselected into another memory DC.

And, before performing any other operations on the bitmap data, the bitmap must be
selected out of the memory DC:

This happens automatically when `m:wxMemoryDC` object goes out of scope.

See:
* `m:wxBitmap`

* `m:wxDC`

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxMemoryDC](https://docs.wxwidgets.org/3.2/classwx_memory_d_c.html)

# `wxMemoryDC`

```erlang
-type wxMemoryDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxMemoryDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxMemoryDC().
```

Constructs a new memory device context.

Use the `wxDC:isOk/1` member to test whether the constructor was successful in creating a usable
device context. Don't forget to select a bitmap into the DC before drawing on it.

# `new`

```erlang
-spec new(Dc) -> wxMemoryDC() when Dc :: wxDC:wxDC() | wxBitmap:wxBitmap().
```

Constructs a new memory device context having the same characteristics as the given
existing device context.

This constructor creates a memory device context `compatible` with `dc` in wxMSW, the
argument is ignored in the other ports. If `dc` is NULL, a device context compatible with
the screen is created, just as with the default constructor.

# `selectObject`

```erlang
-spec selectObject(This, Bitmap) -> ok when This :: wxMemoryDC(), Bitmap :: wxBitmap:wxBitmap().
```

Works exactly like `selectObjectAsSource/2` but this is the function you should use when
you select a bitmap because you want to modify it, e.g.

drawing on this DC.

Using `selectObjectAsSource/2` when modifying the bitmap may incur some problems related to `m:wxBitmap` being a
reference counted object (see overview_refcount).

Before using the updated bitmap data, make sure to select it out of context first either
by selecting ?wxNullBitmap into the device context or destroying the device context entirely.

If the bitmap is already selected in this device context, nothing is done. If it is
selected in another context, the function asserts and drawing on the bitmap won't work correctly.

See: `wxDC:drawBitmap/4`

# `selectObjectAsSource`

```erlang
-spec selectObjectAsSource(This, Bitmap) -> ok when This :: wxMemoryDC(), Bitmap :: wxBitmap:wxBitmap().
```

Selects the given bitmap into the device context, to use as the memory bitmap.

Selecting the bitmap into a memory DC allows you to draw into the DC (and therefore the
bitmap) and also to use `wxDC:blit/6` to copy the bitmap to a window. For this purpose, you may find `wxDC:drawIcon/3`
easier to use instead.

If the argument is ?wxNullBitmap (or some other uninitialised `m:wxBitmap`) the current
bitmap is selected out of the device context, and the original bitmap restored, allowing
the current bitmap to be destroyed safely.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
