# `wxMenuItem`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxMenuItem.erl#L58)

A menu item represents an item in a menu.

Note that you usually don't have to deal with it directly as `m:wxMenu` methods usually
construct an object of this class for you.

Also please note that the methods related to fonts and bitmaps are currently only
implemented for Windows, Mac and GTK+.

See:
* `m:wxMenuBar`

* `m:wxMenu`

wxWidgets docs: [wxMenuItem](https://docs.wxwidgets.org/3.2/classwx_menu_item.html)

## Events

Event types emitted from this class:

* [`menu_open`](`m:wxMenuEvent`)

* [`menu_close`](`m:wxMenuEvent`)

* [`menu_highlight`](`m:wxMenuEvent`)

# `wxMenuItem`

```elixir
-type wxMenuItem() :: wx:wx_object().
```

# `check`

```elixir
-spec check(This) -> ok when This :: wxMenuItem().
```

# `check`

```elixir
-spec check(This, [Option]) -> ok when This :: wxMenuItem(), Option :: {check, boolean()}.
```

Checks or unchecks the menu item.

Note that this only works when the item is already appended to a menu.

# `destroy`

```elixir
-spec destroy(This :: wxMenuItem()) -> ok.
```

Destroys the object

# `enable`

```elixir
-spec enable(This) -> ok when This :: wxMenuItem().
```

# `enable`

```elixir
-spec enable(This, [Option]) -> ok when This :: wxMenuItem(), Option :: {enable, boolean()}.
```

Enables or disables the menu item.

# `getBitmap`

```elixir
-spec getBitmap(This) -> wxBitmap:wxBitmap() when This :: wxMenuItem().
```

Returns the checked or unchecked bitmap.

Only for:wxmsw

# `getHelp`

```elixir
-spec getHelp(This) -> unicode:charlist() when This :: wxMenuItem().
```

Returns the help string associated with the menu item.

# `getId`

```elixir
-spec getId(This) -> integer() when This :: wxMenuItem().
```

Returns the menu item identifier.

# `getItemLabel`

```elixir
-spec getItemLabel(This) -> unicode:charlist() when This :: wxMenuItem().
```

Returns the text associated with the menu item including any accelerator characters that
were passed to the constructor or `setItemLabel/2`.

See:
* `getItemLabelText/1`

* `getLabelText/1`

# `getItemLabelText`

```elixir
-spec getItemLabelText(This) -> unicode:charlist() when This :: wxMenuItem().
```

Returns the text associated with the menu item, without any accelerator characters.

See:
* `getItemLabel/1`

* `getLabelText/1`

# `getKind`

```elixir
-spec getKind(This) -> wx:wx_enum() when This :: wxMenuItem().
```

Returns the item kind, one of `wxITEM_SEPARATOR`, `wxITEM_NORMAL`, `wxITEM_CHECK` or `wxITEM_RADIO`.

# `getLabel`

```elixir
-spec getLabel(This) -> unicode:charlist() when This :: wxMenuItem().
```

Equivalent to: `getItemLabelText/1`

# `getLabelFromText`

```elixir
-spec getLabelFromText(Text) -> unicode:charlist() when Text :: unicode:chardata().
```

Equivalent to: `getLabelText/1`

# `getLabelText`

```elixir
-spec getLabelText(Text) -> unicode:charlist() when Text :: unicode:chardata().
```

Strips all accelerator characters and mnemonics from the given `text`.

For example:

will return just `"Hello"`.

See:
* `getItemLabelText/1`

* `getItemLabel/1`

# `getMenu`

```elixir
-spec getMenu(This) -> wxMenu:wxMenu() when This :: wxMenuItem().
```

Returns the menu this menu item is in, or NULL if this menu item is not attached.

# `getSubMenu`

```elixir
-spec getSubMenu(This) -> wxMenu:wxMenu() when This :: wxMenuItem().
```

Returns the submenu associated with the menu item, or NULL if there isn't one.

# `getText`

```elixir
-spec getText(This) -> unicode:charlist() when This :: wxMenuItem().
```

Equivalent to: `getItemLabel/1`

# `isCheckable`

```elixir
-spec isCheckable(This) -> boolean() when This :: wxMenuItem().
```

Returns true if the item is checkable.

Notice that the radio buttons are considered to be checkable as well, so this method
returns true for them too. Use `IsCheck()` (not implemented in wx) if you want to test for
the check items only.

# `isChecked`

```elixir
-spec isChecked(This) -> boolean() when This :: wxMenuItem().
```

Returns true if the item is checked.

# `isEnabled`

```elixir
-spec isEnabled(This) -> boolean() when This :: wxMenuItem().
```

Returns true if the item is enabled.

# `isSeparator`

```elixir
-spec isSeparator(This) -> boolean() when This :: wxMenuItem().
```

Returns true if the item is a separator.

# `isSubMenu`

```elixir
-spec isSubMenu(This) -> boolean() when This :: wxMenuItem().
```

Returns true if the item is a submenu.

# `new`

```elixir
-spec new() -> wxMenuItem().
```

# `new`

```elixir
-spec new([Option]) -> wxMenuItem()
             when
                 Option ::
                     {parentMenu, wxMenu:wxMenu()} |
                     {id, integer()} |
                     {text, unicode:chardata()} |
                     {help, unicode:chardata()} |
                     {kind, wx:wx_enum()} |
                     {subMenu, wxMenu:wxMenu()}.
```

Constructs a `m:wxMenuItem` object.

Menu items can be standard, or "stock menu items", or custom. For the standard menu items
(such as commands to open a file, exit the program and so on, see page_stockitems for the
full list) it is enough to specify just the stock ID and leave `text` and `help` string
empty. Some platforms (currently wxGTK only, and see the remark in `setBitmap/2` documentation) will
also show standard bitmaps for stock menu items.

Leaving at least `text` empty for the stock menu items is actually strongly recommended
as they will have appearance and keyboard interface (including standard accelerators)
familiar to the user.

For the custom (non-stock) menu items, `text` must be specified and while `help` string
may be left empty, it's recommended to pass the item description (which is automatically
shown by the library in the status bar when the menu item is selected) in this parameter.

Finally note that you can e.g. use a stock menu label without using its stock help string:

that is, stock properties are set independently one from the other.

# `setBitmap`

```elixir
-spec setBitmap(This, Bmp) -> ok when This :: wxMenuItem(), Bmp :: wxBitmap:wxBitmap().
```

Sets the bitmap for the menu item.

It is equivalent to wxMenuItem::SetBitmaps(bmp, wxNullBitmap) if `checked` is true
(default value) or SetBitmaps(wxNullBitmap, bmp) otherwise.

`setBitmap/2` must be called before the item is appended to the menu, i.e. appending the item without
a bitmap and setting one later is not guaranteed to work. But the bitmap can be changed or
reset later if it had been set up initially.

Notice that GTK+ uses a global setting called `gtk-menu-images` to determine if the
images should be shown in the menus at all. If it is off (which is the case in e.g. Gnome
2.28 by default), no images will be shown, consistently with the native behaviour.

Only for:wxmsw,wxosx,wxgtk

# `setHelp`

```elixir
-spec setHelp(This, HelpString) -> ok when This :: wxMenuItem(), HelpString :: unicode:chardata().
```

Sets the help string.

# `setItemLabel`

```elixir
-spec setItemLabel(This, Label) -> ok when This :: wxMenuItem(), Label :: unicode:chardata().
```

Sets the label associated with the menu item.

Note that if the ID of this menu item corresponds to a stock ID, then it is not necessary
to specify a label: wxWidgets will automatically use the stock item label associated with
that ID. See the `new/1` for more info.

The label string for the normal menu items (not separators) may include the accelerator
which can be used to activate the menu item from keyboard. An accelerator key can be
specified using the ampersand `&` character. In order to embed an ampersand character in
the menu item text, the ampersand must be doubled.

Optionally you can specify also an accelerator string appending a tab character `\t`
followed by a valid key combination (e.g. `CTRL+V`). Its general syntax is any combination
of `"CTRL"`, `"RAWCTRL"`, `"ALT"` and `"SHIFT"` strings (case doesn't matter) separated by
either `'-'` or `'+'` characters and followed by the accelerator itself. Notice that `CTRL`
corresponds to the "Ctrl" key on most platforms but not under macOS where it is mapped to
"Cmd" key on Mac keyboard. Usually this is exactly what you want in portable code but if
you really need to use the (rarely used for this purpose) "Ctrl" key even under Mac, you
may use `RAWCTRL` to prevent this mapping. Under the other platforms `RAWCTRL` is the same
as plain `CTRL`.

The accelerator may be any alphanumeric character, any function key (from `F1` to `F12`),
any numpad digit key using `KP_` prefix (i.e. from `KP_0` to `KP_9`) or one of the special
strings listed below (again, case doesn't matter) corresponding to the specified key code:

* `Del` or `Delete:` WXK_DELETE

* `Back:` WXK_BACK

* `Ins` or `Insert:` WXK_INSERT

* `Enter` or `Return:` WXK_RETURN

* `PgUp` or `PageUp:` WXK_PAGEUP

* `PgDn` or `PageDown:` WXK_PAGEDOWN

* `Left:` WXK_LEFT

* `Right:` WXK_RIGHT

* `Up:` WXK_UP

* `Down:` WXK_DOWN

* `Home:` WXK_HOME

* `End:` WXK_END

* `Space:` WXK_SPACE

* `Tab:` WXK_TAB

* `Esc` or `Escape:` WXK_ESCAPE

* `Cancel:` WXK_CANCEL

* `Clear:` WXK_CLEAR

* `Menu:` WXK_MENU

* `Pause:` WXK_PAUSE

* `Capital:` WXK_CAPITAL

* `Select:` WXK_SELECT

* `Print:` WXK_PRINT

* `Execute:` WXK_EXECUTE

* `Snapshot:` WXK_SNAPSHOT

* `Help:` WXK_HELP

* `Add:` WXK_ADD

* `Separator:` WXK_SEPARATOR

* `Subtract:` WXK_SUBTRACT

* `Decimal:` WXK_DECIMAL

* `Divide:` WXK_DIVIDE

* `Num_lock:` WXK_NUMLOCK

* `Scroll_lock:` WXK_SCROLL

* `KP_Space:` WXK_NUMPAD_SPACE

* `KP_Tab:` WXK_NUMPAD_TAB

* `KP_Enter:` WXK_NUMPAD_ENTER

* `KP_Home:` WXK_NUMPAD_HOME

* `KP_Left:` WXK_NUMPAD_LEFT

* `KP_Up:` WXK_NUMPAD_UP

* `KP_Right:` WXK_NUMPAD_RIGHT

* `KP_Down:` WXK_NUMPAD_DOWN

* `KP_PageUp:` WXK_NUMPAD_PAGEUP

* `KP_PageDown:` WXK_NUMPAD_PAGEDOWN

* `KP_Prior:` WXK_NUMPAD_PAGEUP

* `KP_Next:` WXK_NUMPAD_PAGEDOWN

* `KP_End:` WXK_NUMPAD_END

* `KP_Begin:` WXK_NUMPAD_BEGIN

* `KP_Insert:` WXK_NUMPAD_INSERT

* `KP_Delete:` WXK_NUMPAD_DELETE

* `KP_Equal:` WXK_NUMPAD_EQUAL

* `KP_Multiply:` WXK_NUMPAD_MULTIPLY

* `KP_Add:` WXK_NUMPAD_ADD

* `KP_Separator:` WXK_NUMPAD_SEPARATOR

* `KP_Subtract:` WXK_NUMPAD_SUBTRACT

* `KP_Decimal:` WXK_NUMPAD_DECIMAL

* `KP_Divide:` WXK_NUMPAD_DIVIDE

* `Windows_Left:` WXK_WINDOWS_LEFT

* `Windows_Right:` WXK_WINDOWS_RIGHT

* `Windows_Menu:` WXK_WINDOWS_MENU

* `Command:` WXK_COMMAND

Examples:

Note: In wxGTK using `"SHIFT"` with non-alphabetic characters currently doesn't work,
even in combination with other modifiers, due to GTK+ limitation. E.g. `Shift+Ctrl+A`
works but `Shift+Ctrl+1` or `Shift+/` do not, so avoid using accelerators of this form in
portable code.

Note: In wxGTk, the left/right/up/down arrow keys do not work as accelerator keys for a
menu item unless a modifier key is used. Additionally, the following keycodes are not
supported as menu accelerator keys:

* WXK_COMMAND/WXK_CONTROL

* WXK_SHIFT

* WXK_ALT

* WXK_SCROLL

* WXK_CAPITAL

* WXK_NUMLOCK

* WXK_NUMPAD_TAB

* WXK_TAB

* WXK_WINDOWS_LEFT

* WXK_WINDOWS_RIGHT

* WXK_ADD

* WXK_SEPARATOR

* WXK_SUBTRACT

* WXK_DECIMAL

* WXK_DIVIDE

* WXK_SNAPSHOT

See:
* `getItemLabel/1`

* `getItemLabelText/1`

# `setMenu`

```elixir
-spec setMenu(This, Menu) -> ok when This :: wxMenuItem(), Menu :: wxMenu:wxMenu().
```

Sets the parent menu which will contain this menu item.

# `setSubMenu`

```elixir
-spec setSubMenu(This, Menu) -> ok when This :: wxMenuItem(), Menu :: wxMenu:wxMenu().
```

Sets the submenu of this menu item.

# `setText`

```elixir
-spec setText(This, Label) -> ok when This :: wxMenuItem(), Label :: unicode:chardata().
```

Equivalent to: `setItemLabel/2`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
