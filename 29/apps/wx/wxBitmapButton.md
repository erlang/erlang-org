# `wxBitmapButton`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxBitmapButton.erl#L58)

A bitmap button is a control that contains a bitmap.

Notice that since wxWidgets 2.9.1 bitmap display is supported by the base `m:wxButton`
class itself and the only tiny advantage of using this class is that it allows specifying
the bitmap in its constructor, unlike `m:wxButton`. Please see the base class
documentation for more information about images support in `m:wxButton`.

## Styles

This class supports the following styles:

* wxBU_LEFT: Left-justifies the bitmap label.

* wxBU_TOP: Aligns the bitmap label to the top of the button.

* wxBU_RIGHT: Right-justifies the bitmap label.

* wxBU_BOTTOM: Aligns the bitmap label to the bottom of the button. Note that the
wxBU_EXACTFIT style supported by `m:wxButton` is not used by this class as bitmap buttons
don't have any minimal standard size by default.

See: `m:wxButton`

This class is derived, and can use functions, from:

* `m:wxButton`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxBitmapButton](https://docs.wxwidgets.org/3.2/classwx_bitmap_button.html)

## Events

Event types emitted from this class:

* [`command_button_clicked`](`m:wxCommandEvent`)

# `wxBitmapButton`

```erlang
-type wxBitmapButton() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Bitmap) -> boolean()
                when
                    This :: wxBitmapButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Bitmap :: wxBitmap:wxBitmap().
```

# `create`

```erlang
-spec create(This, Parent, Id, Bitmap, [Option]) -> boolean()
                when
                    This :: wxBitmapButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Bitmap :: wxBitmap:wxBitmap(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Button creation function for two-step creation.

For more details, see `new/4`.

# `destroy`

```erlang
-spec destroy(This :: wxBitmapButton()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxBitmapButton().
```

Default ctor.

# `new`

```erlang
-spec new(Parent, Id, Bitmap) -> wxBitmapButton()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Bitmap :: wxBitmap:wxBitmap().
```

# `new`

```erlang
-spec new(Parent, Id, Bitmap, [Option]) -> wxBitmapButton()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Bitmap :: wxBitmap:wxBitmap(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a button.

Remark: The bitmap parameter is normally the only bitmap you need to provide, and
wxWidgets will draw the button correctly in its different states. If you want more
control, call any of the functions `SetBitmapPressed()` (not implemented in wx), `wxButton:setBitmapFocus/2`, `wxButton:setBitmapDisabled/2`.

See: `create/5`

# `newCloseButton`

```erlang
-spec newCloseButton(Parent, Winid) -> wxBitmapButton()
                        when Parent :: wxWindow:wxWindow(), Winid :: integer().
```

Helper function creating a standard-looking "Close" button.

To get the best results, platform-specific code may need to be used to create a small,
title bar-like "Close" button. This function is provided to avoid the need to test for the
current platform and creates the button with as native look as possible.

Return: The new button.

Since: 2.9.5

---

*Consult [api-reference.md](api-reference.md) for complete listing*
