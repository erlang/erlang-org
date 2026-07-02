# `wxButton`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxButton.erl#L58)

A button is a control that contains a text string, and is one of the most common elements
of a GUI.

It may be placed on a `m:wxDialog` or on a `m:wxPanel` panel, or indeed on almost any
other window.

By default, i.e. if none of the alignment styles are specified, the label is centered
both horizontally and vertically. If the button has both a label and a bitmap, the
alignment styles above specify the location of the rectangle combining both the label and
the bitmap and the bitmap position set with `wxButton::SetBitmapPosition()` (not
implemented in wx) defines the relative position of the bitmap with respect to the label
(however currently non-default alignment combinations are not implemented on all platforms).

Since version 2.9.1 `m:wxButton` supports showing both text and an image (currently only
when using wxMSW, wxGTK or wxOSX/Cocoa ports), see `SetBitmap()` (not implemented in wx)
and `setBitmapLabel/2`, `setBitmapDisabled/2` &c methods. In the previous wxWidgets versions this functionality was only
available in (the now trivial) `m:wxBitmapButton` class which was only capable of showing
an image without text.

A button may have either a single image for all states or different images for the
following states (different images are not currently supported under macOS where the
normal image is used for all states):

* `normal:` the default state

* `disabled:` bitmap shown when the button is disabled.

* `pressed:` bitmap shown when the button is pushed (e.g. while the user keeps the mouse
button pressed on it)

* `focus:` bitmap shown when the button has keyboard focus (but is not pressed as in this
case the button is in the pressed state)

* `current:` bitmap shown when the mouse is over the button (but it is not pressed although
it may have focus). Notice that if current bitmap is not specified but the current
platform UI uses hover images for the buttons (such as Windows or GTK+), then the focus
bitmap is used for hover state as well. This makes it possible to set focus bitmap only to
get reasonably good behaviour on all platforms.

All of the bitmaps must be of the same size and the normal bitmap must be set first (to
a valid bitmap), before setting any other ones. Also, if the size of the bitmaps is
changed later, you need to change the size of the normal bitmap before setting any other
bitmaps with the new size (and you do need to reset all of them as their original values
can be lost when the normal bitmap size changes).

The position of the image inside the button be configured using `SetBitmapPosition()`
(not implemented in wx). By default the image is on the left of the text.

Please also notice that GTK+ uses a global setting called `gtk-button-images` to
determine if the images should be shown in the buttons at all. If it is off (which is the
case in e.g. Gnome 2.28 by default), no images will be shown, consistently with the native behaviour.

## Styles

This class supports the following styles:

* wxBU_LEFT: Left-justifies the label. Windows and GTK+ only.

* wxBU_TOP: Aligns the label to the top of the button. Windows and GTK+ only.

* wxBU_RIGHT: Right-justifies the bitmap label. Windows and GTK+ only.

* wxBU_BOTTOM: Aligns the label to the bottom of the button. Windows and GTK+ only.

* wxBU_EXACTFIT: By default, all buttons are made of at least the standard button size,
even if their contents is small enough to fit into a smaller size. This is done for
consistency as most platforms use buttons of the same size in the native dialogs, but can
be overridden by specifying this flag. If it is given, the button will be made just big
enough for its contents. Notice that under MSW the button will still have at least the
standard height, even with this style, if it has a non-empty label.

* wxBU_NOTEXT: Disables the display of the text label in the button even if it has one or
its id is one of the standard stock ids with an associated label: without using this style
a button which is only supposed to show a bitmap but uses a standard id would display a
label too.

* wxBORDER_NONE: Creates a button without border. This is currently implemented in MSW,
GTK2 and OSX/Cocoa.

See: `m:wxBitmapButton`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxButton](https://docs.wxwidgets.org/3.2/classwx_button.html)

## Events

Event types emitted from this class:

* [`command_button_clicked`](`m:wxCommandEvent`)

# `wxButton`

```elixir
-type wxButton() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id) -> boolean()
                when This :: wxButton(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```elixir
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxButton(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {label, unicode:chardata()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Button creation function for two-step creation.

For more details, see `new/3`.

# `destroy`

```elixir
-spec destroy(This :: wxButton()) -> ok.
```

Destroys the object

# `getBitmapDisabled`

```elixir
-spec getBitmapDisabled(This) -> wxBitmap:wxBitmap() when This :: wxButton().
```

Returns the bitmap for the disabled state, which may be invalid.

See: `setBitmapDisabled/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `getBitmapFocus`

```elixir
-spec getBitmapFocus(This) -> wxBitmap:wxBitmap() when This :: wxButton().
```

Returns the bitmap for the focused state, which may be invalid.

See: `setBitmapFocus/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `getBitmapLabel`

```elixir
-spec getBitmapLabel(This) -> wxBitmap:wxBitmap() when This :: wxButton().
```

Returns the bitmap for the normal state.

This is exactly the same as `GetBitmap()` (not implemented in wx) but uses a name
backwards-compatible with `m:wxBitmapButton`.

See: `setBitmapLabel/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `getDefaultSize`

```elixir
-spec getDefaultSize() -> {W :: integer(), H :: integer()}.
```

Returns the default size for the buttons.

It is advised to make all the dialog buttons of the same size and this function allows
retrieving the (platform, and current font dependent) size which should be the best suited
for this.

The optional `win` argument is new since wxWidgets 3.1.3 and allows to get a per-monitor
DPI specific size.

# `getDefaultSize`

```elixir
-spec getDefaultSize(Win) -> {W :: integer(), H :: integer()} when Win :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new() -> wxButton().
```

Default ctor.

# `new`

```elixir
-spec new(Parent, Id) -> wxButton() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```elixir
-spec new(Parent, Id, [Option]) -> wxButton()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {label, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a button.

The preferred way to create standard buttons is to use default value of `label`. If no
label is supplied and `id` is one of standard IDs from this list, a standard label will be
used. In other words, if you use a predefined `wxID_XXX` constant, just omit the label
completely rather than specifying it. In particular, help buttons (the ones with `id` of `wxID_HELP`)
under macOS can't display any label at all and while `m:wxButton` will detect if the
standard "Help" label is used and ignore it, using any other label will prevent the button
from correctly appearing as a help button and so should be avoided.

In addition to that, the button will be decorated with stock icons under GTK+ 2.

See: `create/4`

# `setBitmapDisabled`

```elixir
-spec setBitmapDisabled(This, Bitmap) -> ok when This :: wxButton(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the bitmap for the disabled button appearance.

If `bitmap` is invalid, the disabled bitmap is set to the automatically generated greyed
out version of the normal bitmap, i.e. the same bitmap as is used by default if this
method is not called at all. Use `SetBitmap()` (not implemented in wx) with an invalid
bitmap to remove the bitmap completely (for all states).

See:
* `getBitmapDisabled/1`

* `setBitmapLabel/2`

* `setBitmapFocus/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `setBitmapFocus`

```elixir
-spec setBitmapFocus(This, Bitmap) -> ok when This :: wxButton(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the bitmap for the button appearance when it has the keyboard focus.

If `bitmap` is invalid, the normal bitmap will be used in the focused state.

See:
* `getBitmapFocus/1`

* `setBitmapLabel/2`

* `setBitmapDisabled/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `setBitmapLabel`

```elixir
-spec setBitmapLabel(This, Bitmap) -> ok when This :: wxButton(), Bitmap :: wxBitmap:wxBitmap().
```

Sets the bitmap label for the button.

Remark: This is the bitmap used for the unselected state, and for all other states if no
other bitmaps are provided.

See: `getBitmapLabel/1`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)

# `setDefault`

```elixir
-spec setDefault(This) -> wxWindow:wxWindow() when This :: wxButton().
```

This sets the button to be the default item in its top-level window (e.g.

the panel or the dialog box containing it).

As normal, pressing return causes the default button to be depressed when the return key
is pressed.

See also `wxWindow:setFocus/1` which sets the keyboard focus for windows and text panel items, and `wxTopLevelWindow::SetDefaultItem()`
(not implemented in wx).

Remark: Under Windows, only dialog box buttons respond to this function.

Return: the old default item (possibly NULL)

# `setLabel`

```elixir
-spec setLabel(This, Label) -> ok when This :: wxButton(), Label :: unicode:chardata().
```

Sets the string label for the button.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
