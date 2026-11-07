# `wxMenuBar`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMenuBar.erl#L58)

A menu bar is a series of menus accessible from the top of a frame.

Remark: To respond to a menu selection, provide a handler for EVT_MENU, in the frame that
contains the menu bar.

If you have a toolbar which uses the same identifiers as your EVT_MENU entries, events
from the toolbar will also be processed by your EVT_MENU event handlers.

Tip: under Windows, if you discover that menu shortcuts (for example, Alt-F to show the
file menu) are not working, check any EVT_CHAR events you are handling in child windows.
If you are not calling event.Skip() for events that you don't process in these event
handlers, menu shortcuts may cease to work.

See:
* `m:wxMenu`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMenuBar](https://docs.wxwidgets.org/3.2/classwx_menu_bar.html)

# `wxMenuBar`

```erlang
-type wxMenuBar() :: wx:wx_object().
```

# `append`

```erlang
-spec append(This, Menu, Title) -> boolean()
                when This :: wxMenuBar(), Menu :: wxMenu:wxMenu(), Title :: unicode:chardata().
```

Adds the item to the end of the menu bar.

Return: true on success, false if an error occurred.

See: `insert/4`

# `check`

```erlang
-spec check(This, Id, Check) -> ok when This :: wxMenuBar(), Id :: integer(), Check :: boolean().
```

Checks or unchecks a menu item.

Remark: Only use this when the menu bar has been associated with a frame; otherwise, use
the `m:wxMenu` equivalent call.

# `destroy`

```erlang
-spec destroy(This :: wxMenuBar()) -> ok.
```

Destroys the object

# `enable`

```erlang
-spec enable(This, Id, Enable) -> ok when This :: wxMenuBar(), Id :: integer(), Enable :: boolean().
```

Enables or disables (greys out) a menu item.

Remark: Only use this when the menu bar has been associated with a frame; otherwise, use
the `m:wxMenu` equivalent call.

# `enableTop`

```erlang
-spec enableTop(This, Pos, Enable) -> ok when This :: wxMenuBar(), Pos :: integer(), Enable :: boolean().
```

Enables or disables a whole menu.

Remark: Only use this when the menu bar has been associated with a frame.

# `findItem`

```erlang
-spec findItem(This, Id) -> wxMenuItem:wxMenuItem() when This :: wxMenuBar(), Id :: integer().
```

Finds the menu item object associated with the given menu item identifier.

Return: The found menu item object, or NULL if one was not found.

# `findMenu`

```erlang
-spec findMenu(This, Title) -> integer() when This :: wxMenuBar(), Title :: unicode:chardata().
```

Returns the index of the menu with the given `title` or `wxNOT\_FOUND` if no such menu
exists in this menubar.

The `title` parameter may specify either the menu title (with accelerator characters,
i.e. `"&File"`) or just the menu label (`"File"`) indifferently.

# `findMenuItem`

```erlang
-spec findMenuItem(This, MenuString, ItemString) -> integer()
                      when
                          This :: wxMenuBar(),
                          MenuString :: unicode:chardata(),
                          ItemString :: unicode:chardata().
```

Finds the menu item id for a menu name/menu item string pair.

Return: The menu item identifier, or wxNOT_FOUND if none was found.

Remark: Any special menu codes are stripped out of source and target strings before
matching.

# `getAutoWindowMenu`

```erlang
-spec getAutoWindowMenu() -> boolean().
```

# `getHelpString`

```erlang
-spec getHelpString(This, Id) -> unicode:charlist() when This :: wxMenuBar(), Id :: integer().
```

Gets the help string associated with the menu item identifier.

Return: The help string, or the empty string if there was no help string or the menu item
was not found.

See: `setHelpString/3`

# `getLabel`

```erlang
-spec getLabel(This, Id) -> unicode:charlist() when This :: wxMenuBar(), Id :: integer().
```

Gets the label associated with a menu item.

Return: The menu item label, or the empty string if the item was not found.

Remark: Use only after the menubar has been associated with a frame.

# `getLabelTop`

```erlang
-spec getLabelTop(This, Pos) -> unicode:charlist() when This :: wxMenuBar(), Pos :: integer().
```

Equivalent to: `getMenuLabel/2`

# `getMenu`

```erlang
-spec getMenu(This, MenuIndex) -> wxMenu:wxMenu() when This :: wxMenuBar(), MenuIndex :: integer().
```

Returns the menu at `menuIndex` (zero-based).

# `getMenuCount`

```erlang
-spec getMenuCount(This) -> integer() when This :: wxMenuBar().
```

Returns the number of menus in this menubar.

# `getMenuLabel`

```erlang
-spec getMenuLabel(This, Pos) -> unicode:charlist() when This :: wxMenuBar(), Pos :: integer().
```

Returns the label of a top-level menu.

Note that the returned string includes the accelerator characters that have been
specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See:
* `getMenuLabelText/2`

* `setMenuLabel/3`

# `getMenuLabelText`

```erlang
-spec getMenuLabelText(This, Pos) -> unicode:charlist() when This :: wxMenuBar(), Pos :: integer().
```

Returns the label of a top-level menu.

Note that the returned string does not include any accelerator characters that may have
been specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See:
* `getMenuLabel/2`

* `setMenuLabel/3`

# `insert`

```erlang
-spec insert(This, Pos, Menu, Title) -> boolean()
                when
                    This :: wxMenuBar(),
                    Pos :: integer(),
                    Menu :: wxMenu:wxMenu(),
                    Title :: unicode:chardata().
```

Inserts the menu at the given position into the menu bar.

Inserting menu at position 0 will insert it in the very beginning of it, inserting at
position `getMenuCount/1` is the same as calling `append/3`.

Return: true on success, false if an error occurred.

See: `append/3`

# `isChecked`

```erlang
-spec isChecked(This, Id) -> boolean() when This :: wxMenuBar(), Id :: integer().
```

Determines whether an item is checked.

Return: true if the item was found and is checked, false otherwise.

# `isEnabled`

```erlang
-spec isEnabled(This, Id) -> boolean() when This :: wxMenuBar(), Id :: integer().
```

Determines whether an item is enabled.

Return: true if the item was found and is enabled, false otherwise.

# `macGetCommonMenuBar`

```erlang
-spec macGetCommonMenuBar() -> wxMenuBar().
```

Enables you to get the global menubar on Mac, that is, the menubar displayed when the app
is running without any frames open.

Return: The global menubar.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx

# `macSetCommonMenuBar`

```erlang
-spec macSetCommonMenuBar(Menubar) -> ok when Menubar :: wxMenuBar().
```

Enables you to set the global menubar on Mac, that is, the menubar displayed when the app
is running without any frames open.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx

# `new`

```erlang
-spec new() -> wxMenuBar().
```

Construct an empty menu bar.

# `new`

```erlang
-spec new(Style) -> wxMenuBar() when Style :: integer().
```

# `oSXGetAppleMenu`

```erlang
-spec oSXGetAppleMenu(This) -> wxMenu:wxMenu() when This :: wxMenuBar().
```

Returns the Apple menu.

This is the leftmost menu with application's name as its title. You shouldn't remove any
items from it, but it is safe to insert extra menu items or submenus into it.

Only for:wxosx

Since: 3.0.1

# `remove`

```erlang
-spec remove(This, Pos) -> wxMenu:wxMenu() when This :: wxMenuBar(), Pos :: integer().
```

Removes the menu from the menu bar and returns the menu object - the caller is
responsible for deleting it.

This function may be used together with `insert/4` to change the menubar dynamically.

See: `replace/4`

# `replace`

```erlang
-spec replace(This, Pos, Menu, Title) -> wxMenu:wxMenu()
                 when
                     This :: wxMenuBar(),
                     Pos :: integer(),
                     Menu :: wxMenu:wxMenu(),
                     Title :: unicode:chardata().
```

Replaces the menu at the given position with another one.

Return: The menu which was previously at position pos. The caller is responsible for
deleting it.

See:
* `insert/4`

* `remove/2`

# `setAutoWindowMenu`

```erlang
-spec setAutoWindowMenu(Enable) -> ok when Enable :: boolean().
```

# `setHelpString`

```erlang
-spec setHelpString(This, Id, HelpString) -> ok
                       when This :: wxMenuBar(), Id :: integer(), HelpString :: unicode:chardata().
```

Sets the help string associated with a menu item.

See: `getHelpString/2`

# `setLabel`

```erlang
-spec setLabel(This, Id, Label) -> ok
                  when This :: wxMenuBar(), Id :: integer(), Label :: unicode:chardata().
```

Sets the label of a menu item.

Remark: Use only after the menubar has been associated with a frame.

See: `getLabel/2`

# `setLabelTop`

```erlang
-spec setLabelTop(This, Pos, Label) -> ok
                     when This :: wxMenuBar(), Pos :: integer(), Label :: unicode:chardata().
```

Equivalent to: `setMenuLabel/3`

# `setMenuLabel`

```erlang
-spec setMenuLabel(This, Pos, Label) -> ok
                      when This :: wxMenuBar(), Pos :: integer(), Label :: unicode:chardata().
```

Sets the label of a top-level menu.

Remark: Use only after the menubar has been associated with a frame.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
