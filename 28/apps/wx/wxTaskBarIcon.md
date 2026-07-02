# `wxTaskBarIcon`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxTaskBarIcon.erl#L58)

This class represents a taskbar icon.

A taskbar icon is an icon that appears in the 'system tray' and responds to mouse clicks,
optionally with a tooltip above it to help provide information.

X Window System Note

Under X Window System, the window manager must support either the "System Tray Protocol"
(see [http://freedesktop.org/wiki/Specifications/systemtray-spec](http://freedesktop.org/wiki/Specifications/systemtray-spec))
by freedesktop.org (WMs used by modern desktop environments such as GNOME >= 2, KDE >= 3
and XFCE >= 4 all do) or the older methods used in GNOME 1.2 and KDE 1 and 2.

If it doesn't, the icon will appear as a toplevel window on user's desktop. Because not
all window managers have system tray, there's no guarantee that `m:wxTaskBarIcon` will
work correctly under X Window System and so the applications should use it only as an
optional component of their user interface. The user should be required to explicitly
enable the taskbar icon on Unix, it shouldn't be on by default.

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxTaskBarIcon](https://docs.wxwidgets.org/3.2/classwx_task_bar_icon.html)

## Events

Event types emitted from this class:

* [`taskbar_move`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_down`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_up`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_down`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_up`](`m:wxTaskBarIconEvent`)

* [`taskbar_left_dclick`](`m:wxTaskBarIconEvent`)

* [`taskbar_right_dclick`](`m:wxTaskBarIconEvent`)

# `wxTaskBarIcon`

```elixir
-type wxTaskBarIcon() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxTaskBarIcon()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new() -> wxTaskBarIcon().
```

# `new`

```elixir
-spec new([Option]) -> wxTaskBarIcon()
             when Option :: {iconType, wx:wx_enum()} | {createPopupMenu, fun(() -> wxMenu:wxMenu())}.
```

# `popupMenu`

```elixir
-spec popupMenu(This, Menu) -> boolean() when This :: wxTaskBarIcon(), Menu :: wxMenu:wxMenu().
```

Pops up a menu at the current mouse position.

The events can be handled by a class derived from `m:wxTaskBarIcon`.

Note: It is recommended to override `CreatePopupMenu()` (not implemented in wx) callback
instead of calling this method from event handler, because some ports (e.g. wxCocoa) may
not implement `popupMenu/2` and mouse click events at all.

# `removeIcon`

```elixir
-spec removeIcon(This) -> boolean() when This :: wxTaskBarIcon().
```

Removes the icon previously set with `setIcon/3`.

# `setIcon`

```elixir
-spec setIcon(This, Icon) -> boolean() when This :: wxTaskBarIcon(), Icon :: wxIcon:wxIcon().
```

# `setIcon`

```elixir
-spec setIcon(This, Icon, [Option]) -> boolean()
                 when
                     This :: wxTaskBarIcon(),
                     Icon :: wxIcon:wxIcon(),
                     Option :: {tooltip, unicode:chardata()}.
```

Sets the icon, and optional tooltip text.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
