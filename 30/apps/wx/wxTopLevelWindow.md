# `wxTopLevelWindow`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxTopLevelWindow.erl#L58)

`m:wxTopLevelWindow` is a common base class for `m:wxDialog` and `m:wxFrame`.

It is an abstract base class meaning that you never work with objects of this class
directly, but all of its methods are also applicable for the two classes above.

Note that the instances of `m:wxTopLevelWindow` are managed by wxWidgets in the internal
top level window list.

See:
* `m:wxDialog`

* `m:wxFrame`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTopLevelWindow](https://docs.wxwidgets.org/3.2/classwx_top_level_window.html)

## Events

Event types emitted from this class:

* [`maximize`](`m:wxMaximizeEvent`)

* [`move`](`m:wxMoveEvent`)

* [`show`](`m:wxShowEvent`)

# `wxTopLevelWindow`

```erlang
-type wxTopLevelWindow() :: wx:wx_object().
```

# `centerOnScreen`

```erlang
-spec centerOnScreen(This) -> ok when This :: wxTopLevelWindow().
```

# `centerOnScreen`

```erlang
-spec centerOnScreen(This, [Option]) -> ok when This :: wxTopLevelWindow(), Option :: {dir, integer()}.
```

Equivalent to: `centreOnScreen/2`

# `centreOnScreen`

```erlang
-spec centreOnScreen(This) -> ok when This :: wxTopLevelWindow().
```

# `centreOnScreen`

```erlang
-spec centreOnScreen(This, [Option]) -> ok when This :: wxTopLevelWindow(), Option :: {dir, integer()}.
```

Centres the window on screen.

See: `wxWindow:centreOnParent/2`

# `getIcon`

```erlang
-spec getIcon(This) -> wxIcon:wxIcon() when This :: wxTopLevelWindow().
```

Returns the standard icon of the window.

The icon will be invalid if it hadn't been previously set by `setIcon/2`.

See: `getIcons/1`

# `getIcons`

```erlang
-spec getIcons(This) -> wxIconBundle:wxIconBundle() when This :: wxTopLevelWindow().
```

Returns all icons associated with the window, there will be none of them if neither `setIcon/2`
nor `setIcons/2` had been called before.

Use `getIcon/1` to get the main icon of the window.

See: `m:wxIconBundle`

# `getTitle`

```erlang
-spec getTitle(This) -> unicode:charlist() when This :: wxTopLevelWindow().
```

Gets a string containing the window title.

See: `setTitle/2`

# `iconize`

```erlang
-spec iconize(This) -> ok when This :: wxTopLevelWindow().
```

# `iconize`

```erlang
-spec iconize(This, [Option]) -> ok when This :: wxTopLevelWindow(), Option :: {iconize, boolean()}.
```

Iconizes or restores the window.

Note that in wxGTK the change to the window state is not immediate, i.e. `isIconized/1` will typically
return false right after a call to `iconize/2` and its return value will only change after the
control flow returns to the event loop and the notification about the window being really
iconized is received.

See:
* `isIconized/1`

* `m:wxIconizeEvent`

# `isActive`

```erlang
-spec isActive(This) -> boolean() when This :: wxTopLevelWindow().
```

Returns true if this window is currently active, i.e. if the user is currently working
with it.

# `isFullScreen`

```erlang
-spec isFullScreen(This) -> boolean() when This :: wxTopLevelWindow().
```

Returns true if the window is in fullscreen mode.

See: `showFullScreen/3`

# `isIconized`

```erlang
-spec isIconized(This) -> boolean() when This :: wxTopLevelWindow().
```

Returns true if the window is iconized.

# `isMaximized`

```erlang
-spec isMaximized(This) -> boolean() when This :: wxTopLevelWindow().
```

Returns true if the window is maximized.

# `maximize`

```erlang
-spec maximize(This) -> ok when This :: wxTopLevelWindow().
```

# `maximize`

```erlang
-spec maximize(This, [Option]) -> ok when This :: wxTopLevelWindow(), Option :: {maximize, boolean()}.
```

Maximizes or restores the window.

Note that, just as with `iconize/2`, the change to the window state is not immediate in at least
wxGTK port.

See: `iconize/2`

# `requestUserAttention`

```erlang
-spec requestUserAttention(This) -> ok when This :: wxTopLevelWindow().
```

# `requestUserAttention`

```erlang
-spec requestUserAttention(This, [Option]) -> ok
                              when This :: wxTopLevelWindow(), Option :: {flags, integer()}.
```

Use a system-dependent way to attract users attention to the window when it is in
background.

`flags` may have the value of either `?wxUSER\_ATTENTION\_INFO` (default) or `?wxUSER\_ATTENTION\_ERROR`
which results in a more drastic action. When in doubt, use the default value.

Note: This function should normally be only used when the application is not already in foreground.

This function is currently implemented for Win32 where it flashes the window icon in the
taskbar, and for wxGTK with task bars supporting it.

# `setIcon`

```erlang
-spec setIcon(This, Icon) -> ok when This :: wxTopLevelWindow(), Icon :: wxIcon:wxIcon().
```

Sets the icon for this window.

Remark: The window takes a 'copy' of `icon`, but since it uses reference counting, the
copy is very quick. It is safe to delete `icon` after calling this function.

Note: In wxMSW, `icon` must be either 16x16 or 32x32 icon.

See:
* `m:wxIcon`

* `setIcons/2`

# `setIcons`

```erlang
-spec setIcons(This, Icons) -> ok when This :: wxTopLevelWindow(), Icons :: wxIconBundle:wxIconBundle().
```

Sets several icons of different sizes for this window: this allows using different icons
for different situations (e.g.

task switching bar, taskbar, window title bar) instead of scaling, with possibly bad
looking results, the only icon set by `setIcon/2`.

Note: In wxMSW, `icons` must contain a 16x16 or 32x32 icon, preferably both.

See: `m:wxIconBundle`

# `setShape`

```erlang
-spec setShape(This, Region) -> boolean()
                  when
                      This :: wxTopLevelWindow(),
                      Region :: wxRegion:wxRegion() | wxGraphicsPath:wxGraphicsPath().
```

If the platform supports it, sets the shape of the window to that depicted by `region`.

The system will not display or respond to any mouse event for the pixels that lie outside
of the region. To reset the window to the normal rectangular shape simply call `setShape/2` again with
an empty `m:wxRegion`. Returns true if the operation is successful.

This method is available in this class only since wxWidgets 2.9.3, previous versions only
provided it in `m:wxTopLevelWindow`.

Note that windows with non default shape have a fixed size and can't be resized using `wxWindow:setSize/6`.

# `setTitle`

```erlang
-spec setTitle(This, Title) -> ok when This :: wxTopLevelWindow(), Title :: unicode:chardata().
```

Sets the window title.

See: `getTitle/1`

# `showFullScreen`

```erlang
-spec showFullScreen(This, Show) -> boolean() when This :: wxTopLevelWindow(), Show :: boolean().
```

# `showFullScreen`

```erlang
-spec showFullScreen(This, Show, [Option]) -> boolean()
                        when This :: wxTopLevelWindow(), Show :: boolean(), Option :: {style, integer()}.
```

Depending on the value of `show` parameter the window is either shown full screen or
restored to its normal state.

`style` is a bit list containing some or all of the following values, which indicate what
elements of the window to hide in full-screen mode:

* `?wxFULLSCREEN\_NOMENUBAR`

* `?wxFULLSCREEN\_NOTOOLBAR`

* `?wxFULLSCREEN\_NOSTATUSBAR`

* `?wxFULLSCREEN\_NOBORDER`

* `?wxFULLSCREEN\_NOCAPTION`

* `?wxFULLSCREEN\_ALL` (all of the above)

This function has not been tested with MDI frames.

Note: Showing a window full screen also actually `wxWindow:show/2`s the window if it isn't shown.

See: `isFullScreen/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
