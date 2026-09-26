# `wxStaticBitmap`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxStaticBitmap.erl#L58)

A static bitmap control displays a bitmap.

Native implementations on some platforms are only meant for display of the small icons in
the dialog boxes.

If you want to display larger images portably, you may use generic implementation
wxGenericStaticBitmap declared in <wx/generic/statbmpg.h>.

Notice that for the best results, the size of the control should be the same as the size
of the image displayed in it, as happens by default if it's not resized explicitly.
Otherwise, behaviour depends on the platform: under MSW, the bitmap is drawn centred
inside the control, while elsewhere it is drawn at the origin of the control. You can use `SetScaleMode()`
(not implemented in wx) to control how the image is scaled inside the control.

See: `m:wxBitmap`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxStaticBitmap](https://docs.wxwidgets.org/3.2/classwx_static_bitmap.html)

# `wxStaticBitmap`

```erlang
-type wxStaticBitmap() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label) -> boolean()
                when
                    This :: wxStaticBitmap(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: wxBitmap:wxBitmap().
```

# `create`

```erlang
-spec create(This, Parent, Id, Label, [Option]) -> boolean()
                when
                    This :: wxStaticBitmap(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: wxBitmap:wxBitmap(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creation function, for two-step construction.

For details see `new/4`.

# `destroy`

```erlang
-spec destroy(This :: wxStaticBitmap()) -> ok.
```

Destroys the object

# `getBitmap`

```erlang
-spec getBitmap(This) -> wxBitmap:wxBitmap() when This :: wxStaticBitmap().
```

Returns the bitmap currently used in the control.

Notice that this method can be called even if `SetIcon()` (not implemented in wx) had
been used.

See: `setBitmap/2`

# `new`

```erlang
-spec new() -> wxStaticBitmap().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id, Label) -> wxStaticBitmap()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Label :: wxBitmap:wxBitmap().
```

# `new`

```erlang
-spec new(Parent, Id, Label, [Option]) -> wxStaticBitmap()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: wxBitmap:wxBitmap(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating and showing a static bitmap control.

See: `create/5`

# `setBitmap`

```erlang
-spec setBitmap(This, Label) -> ok when This :: wxStaticBitmap(), Label :: wxBitmap:wxBitmap().
```

Sets the bitmap label.

See: `getBitmap/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
