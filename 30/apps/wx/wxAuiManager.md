# `wxAuiManager`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxAuiManager.erl#L58)

`m:wxAuiManager` is the central class of the wxAUI class framework.

`m:wxAuiManager` manages the panes associated with it for a particular `m:wxFrame`, using
a pane's `m:wxAuiPaneInfo` information to determine each pane's docking and floating behaviour.

`m:wxAuiManager` uses wxWidgets' sizer mechanism to plan the layout of each frame. It
uses a replaceable dock art class to do all drawing, so all drawing is localized in one
area, and may be customized depending on an application's specific needs.

`m:wxAuiManager` works as follows: the programmer adds panes to the class, or makes
changes to existing pane properties (dock position, floating state, show state, etc.). To
apply these changes, `m:wxAuiManager`'s `update/1` function is called. This batch processing can be
used to avoid flicker, by modifying more than one pane at a time, and then "committing"
all of the changes at once by calling `update/1`.

Panes can be added quite easily:

Later on, the positions can be modified easily. The following will float an existing pane
in a tool window:

Layers, Rows and Directions, Positions

Inside wxAUI, the docking layout is figured out by checking several pane parameters. Four
of these are important for determining where a pane will end up:

* Direction: Each docked pane has a direction, Top, Bottom, Left, Right, or Center. This is
fairly self-explanatory. The pane will be placed in the location specified by this
variable.

* Position: More than one pane can be placed inside of a dock. Imagine two panes being
docked on the left side of a window. One pane can be placed over another. In
proportionally managed docks, the pane position indicates its sequential position,
starting with zero. So, in our scenario with two panes docked on the left side, the top
pane in the dock would have position 0, and the second one would occupy position 1.

* Row: A row can allow for two docks to be placed next to each other. One of the most
common places for this to happen is in the toolbar. Multiple toolbar rows are allowed, the
first row being row 0, and the second row 1. Rows can also be used on vertically docked
panes.

* Layer: A layer is akin to an onion. Layer 0 is the very center of the managed pane. Thus,
if a pane is in layer 0, it will be closest to the center window (also sometimes known as
the "content window"). Increasing layers "swallow up" all layers of a lower value. This
can look very similar to multiple rows, but is different because all panes in a lower
level yield to panes in higher levels. The best way to understand layers is by running the
wxAUI sample.

## Styles

This class supports the following styles:

* wxAUI_MGR_ALLOW_FLOATING: Allow a pane to be undocked to take the form of a `m:wxMiniFrame`.

* wxAUI_MGR_ALLOW_ACTIVE_PANE: Change the color of the title bar of the pane when it is
activated.

* wxAUI_MGR_TRANSPARENT_DRAG: Make the pane transparent during its movement.

* wxAUI_MGR_TRANSPARENT_HINT: The possible location for docking is indicated by a
translucent area.

* wxAUI_MGR_VENETIAN_BLINDS_HINT: The possible location for docking is indicated by
gradually appearing partially transparent hint.

* wxAUI_MGR_RECTANGLE_HINT: The possible location for docking is indicated by a rectangular
outline.

* wxAUI_MGR_HINT_FADE: The translucent area where the pane could be docked appears
gradually.

* wxAUI_MGR_NO_VENETIAN_BLINDS_FADE: Used in complement of wxAUI_MGR_VENETIAN_BLINDS_HINT
to show the docking hint immediately.

* wxAUI_MGR_LIVE_RESIZE: When a docked pane is resized, its content is refreshed in live
(instead of moving the border alone and refreshing the content at the end).

* wxAUI_MGR_DEFAULT: Default behaviour, combines: wxAUI_MGR_ALLOW_FLOATING |
wxAUI_MGR_TRANSPARENT_HINT | wxAUI_MGR_HINT_FADE | wxAUI_MGR_NO_VENETIAN_BLINDS_FADE.

See:
* [Overview aui](https://docs.wxwidgets.org/3.2/overview_aui.html#overview_aui)

* `m:wxAuiNotebook`

* `m:wxAuiDockArt`

* `m:wxAuiPaneInfo`

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxAuiManager](https://docs.wxwidgets.org/3.2/classwx_aui_manager.html)

## Events

Event types emitted from this class:

* [`aui_pane_button`](`m:wxAuiManagerEvent`)

* [`aui_pane_close`](`m:wxAuiManagerEvent`)

* [`aui_pane_maximize`](`m:wxAuiManagerEvent`)

* [`aui_pane_restore`](`m:wxAuiManagerEvent`)

* [`aui_pane_activated`](`m:wxAuiManagerEvent`)

* [`aui_render`](`m:wxAuiManagerEvent`)

# `wxAuiManager`

```erlang
-type wxAuiManager() :: wx:wx_object().
```

# `addPane`

```erlang
-spec addPane(This, Window) -> boolean() when This :: wxAuiManager(), Window :: wxWindow:wxWindow().
```

# `addPane`

```erlang
-spec addPane(This, Window, [Option]) -> boolean()
                 when
                     This :: wxAuiManager(),
                     Window :: wxWindow:wxWindow(),
                     Option :: {direction, integer()} | {caption, unicode:chardata()};
             (This, Window, Pane_info) -> boolean()
                 when
                     This :: wxAuiManager(),
                     Window :: wxWindow:wxWindow(),
                     Pane_info :: wxAuiPaneInfo:wxAuiPaneInfo().
```

`addPane/4` tells the frame manager to start managing a child window.

There are several versions of this function. The first version allows the full spectrum
of pane parameter possibilities. The second version is used for simpler user interfaces
which do not require as much configuration. The last version allows a drop position to be
specified, which will determine where the pane will be added.

# `addPane`

```erlang
-spec addPane(This, Window, Pane_info, Drop_pos) -> boolean()
                 when
                     This :: wxAuiManager(),
                     Window :: wxWindow:wxWindow(),
                     Pane_info :: wxAuiPaneInfo:wxAuiPaneInfo(),
                     Drop_pos :: {X :: integer(), Y :: integer()}.
```

# `destroy`

```erlang
-spec destroy(This :: wxAuiManager()) -> ok.
```

Destroys the object

# `detachPane`

```erlang
-spec detachPane(This, Window) -> boolean() when This :: wxAuiManager(), Window :: wxWindow:wxWindow().
```

Tells the `m:wxAuiManager` to stop managing the pane specified by window.

The window, if in a floated frame, is reparented to the frame managed by `m:wxAuiManager`.

# `getAllPanes`

```erlang
-spec getAllPanes(This) -> [wxAuiPaneInfo:wxAuiPaneInfo()] when This :: wxAuiManager().
```

Returns an array of all panes managed by the frame manager.

# `getArtProvider`

```erlang
-spec getArtProvider(This) -> wxAuiDockArt:wxAuiDockArt() when This :: wxAuiManager().
```

Returns the current art provider being used.

See: `m:wxAuiDockArt`

# `getDockSizeConstraint`

```erlang
-spec getDockSizeConstraint(This) -> {Widthpct :: number(), Heightpct :: number()}
                               when This :: wxAuiManager().
```

Returns the current dock constraint values.

See `setDockSizeConstraint/3` for more information.

# `getFlags`

```erlang
-spec getFlags(This) -> integer() when This :: wxAuiManager().
```

Returns the current ?wxAuiManagerOption's flags.

# `getManagedWindow`

```erlang
-spec getManagedWindow(This) -> wxWindow:wxWindow() when This :: wxAuiManager().
```

Returns the frame currently being managed by `m:wxAuiManager`.

# `getManager`

```erlang
-spec getManager(Window) -> wxAuiManager() when Window :: wxWindow:wxWindow().
```

Calling this method will return the `m:wxAuiManager` for a given window.

The `window` parameter should specify any child window or sub-child window of the frame
or window managed by `m:wxAuiManager`.

The `window` parameter need not be managed by the manager itself, nor does it even need
to be a child or sub-child of a managed window. It must however be inside the window
hierarchy underneath the managed window.

# `getPane`

```erlang
-spec getPane(This, Name) -> wxAuiPaneInfo:wxAuiPaneInfo()
                 when This :: wxAuiManager(), Name :: unicode:chardata();
             (This, Window) -> wxAuiPaneInfo:wxAuiPaneInfo()
                 when This :: wxAuiManager(), Window :: wxWindow:wxWindow().
```

`getPane/2` is used to lookup a `m:wxAuiPaneInfo` object either by window pointer or by
pane name, which acts as a unique id for a window pane.

The returned `m:wxAuiPaneInfo` object may then be modified to change a pane's look, state
or position. After one or more modifications to `m:wxAuiPaneInfo`, `update/1` should be called to
commit the changes to the user interface. If the lookup failed (meaning the pane could not
be found in the manager), a call to the returned `m:wxAuiPaneInfo`'s IsOk() method will
return false.

# `hideHint`

```erlang
-spec hideHint(This) -> ok when This :: wxAuiManager().
```

`hideHint/1` hides any docking hint that may be visible.

# `insertPane`

```erlang
-spec insertPane(This, Window, Insert_location) -> boolean()
                    when
                        This :: wxAuiManager(),
                        Window :: wxWindow:wxWindow(),
                        Insert_location :: wxAuiPaneInfo:wxAuiPaneInfo().
```

# `insertPane`

```erlang
-spec insertPane(This, Window, Insert_location, [Option]) -> boolean()
                    when
                        This :: wxAuiManager(),
                        Window :: wxWindow:wxWindow(),
                        Insert_location :: wxAuiPaneInfo:wxAuiPaneInfo(),
                        Option :: {insert_level, integer()}.
```

This method is used to insert either a previously unmanaged pane window into the frame
manager, or to insert a currently managed pane somewhere else.

`insertPane/4` will push all panes, rows, or docks aside and insert the window into the position
specified by `insert_location`.

Because `insert_location` can specify either a pane, dock row, or dock layer, the `insert_level`
parameter is used to disambiguate this. The parameter `insert_level` can take a value of
wxAUI_INSERT_PANE, wxAUI_INSERT_ROW or wxAUI_INSERT_DOCK.

# `loadPaneInfo`

```erlang
-spec loadPaneInfo(This, Pane_part, Pane) -> ok
                      when
                          This :: wxAuiManager(),
                          Pane_part :: unicode:chardata(),
                          Pane :: wxAuiPaneInfo:wxAuiPaneInfo().
```

`loadPaneInfo/3` is similar to LoadPerspective, with the exception that it only loads
information about a single pane.

This method writes the serialized data into the passed pane. Pointers to UI elements are
not modified.

Note: This operation also changes the name in the pane information!

See: `loadPerspective/3`

See: `savePaneInfo/2`

See: `savePerspective/1`

# `loadPerspective`

```erlang
-spec loadPerspective(This, Perspective) -> boolean()
                         when This :: wxAuiManager(), Perspective :: unicode:chardata().
```

# `loadPerspective`

```erlang
-spec loadPerspective(This, Perspective, [Option]) -> boolean()
                         when
                             This :: wxAuiManager(),
                             Perspective :: unicode:chardata(),
                             Option :: {update, boolean()}.
```

Loads a saved perspective.

A perspective is the layout state of an AUI managed window.

All currently existing panes that have an object in "perspective" with the same name
("equivalent") will receive the layout parameters of the object in "perspective". Existing
panes that do not have an equivalent in "perspective" remain unchanged, objects in
"perspective" having no equivalent in the manager are ignored.

See: `loadPaneInfo/3`

See: `loadPerspective/3`

See: `savePerspective/1`

# `new`

```erlang
-spec new() -> wxAuiManager().
```

# `new`

```erlang
-spec new([Option]) -> wxAuiManager()
             when Option :: {managed_wnd, wxWindow:wxWindow()} | {flags, integer()}.
```

Constructor.

# `savePaneInfo`

```erlang
-spec savePaneInfo(This, Pane) -> unicode:charlist()
                      when This :: wxAuiManager(), Pane :: wxAuiPaneInfo:wxAuiPaneInfo().
```

`savePaneInfo/2` is similar to SavePerspective, with the exception that it only saves
information about a single pane.

Return: The serialized layout parameters of the pane are returned within the string.
Information about the pointers to UI elements stored in the pane are not serialized.

See: `loadPaneInfo/3`

See: `loadPerspective/3`

See: `savePerspective/1`

# `savePerspective`

```erlang
-spec savePerspective(This) -> unicode:charlist() when This :: wxAuiManager().
```

Saves the entire user interface layout into an encoded `wxString` (not implemented in
wx), which can then be stored by the application (probably using wxConfig).

See: `loadPerspective/3`

See: `loadPaneInfo/3`

See: `savePaneInfo/2`

# `setArtProvider`

```erlang
-spec setArtProvider(This, Art_provider) -> ok
                        when This :: wxAuiManager(), Art_provider :: wxAuiDockArt:wxAuiDockArt().
```

Instructs `m:wxAuiManager` to use art provider specified by parameter `art\_provider` for
all drawing calls.

This allows pluggable look-and-feel features. The previous art provider object, if any,
will be deleted by `m:wxAuiManager`.

See: `m:wxAuiDockArt`

# `setDockSizeConstraint`

```erlang
-spec setDockSizeConstraint(This, Widthpct, Heightpct) -> ok
                               when This :: wxAuiManager(), Widthpct :: number(), Heightpct :: number().
```

When a user creates a new dock by dragging a window into a docked position, often times
the large size of the window will create a dock that is unwieldy large.

`m:wxAuiManager` by default limits the size of any new dock to 1/3 of the window size.
For horizontal docks, this would be 1/3 of the window height. For vertical docks, 1/3 of
the width.

Calling this function will adjust this constraint value. The numbers must be between 0.0
and 1.0. For instance, calling SetDockSizeContraint with 0.5, 0.5 will cause new docks to
be limited to half of the size of the entire managed window.

# `setFlags`

```erlang
-spec setFlags(This, Flags) -> ok when This :: wxAuiManager(), Flags :: integer().
```

This method is used to specify ?wxAuiManagerOption's flags.

`flags` specifies options which allow the frame management behaviour to be modified.

# `setManagedWindow`

```erlang
-spec setManagedWindow(This, Managed_wnd) -> ok
                          when This :: wxAuiManager(), Managed_wnd :: wxWindow:wxWindow().
```

Called to specify the frame or window which is to be managed by `m:wxAuiManager`.

Frame management is not restricted to just frames. Child windows or custom controls are
also allowed.

# `showHint`

```erlang
-spec showHint(This, Rect) -> ok
                  when
                      This :: wxAuiManager(),
                      Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This function is used by controls to explicitly show a hint window at the specified
rectangle.

It is rarely called, and is mostly used by controls implementing custom pane drag/drop
behaviour. The specified rectangle should be in screen coordinates.

# `unInit`

```erlang
-spec unInit(This) -> ok when This :: wxAuiManager().
```

Dissociate the managed window from the manager.

This function may be called before the managed frame or window is destroyed, but, since
wxWidgets 3.1.4, it's unnecessary to call it explicitly, as it will be called
automatically when this window is destroyed, as well as when the manager itself is.

# `update`

```erlang
-spec update(This) -> ok when This :: wxAuiManager().
```

This method is called after any number of changes are made to any of the managed panes.

`update/1` must be invoked after `addPane/4` or `insertPane/4` are called in order to "realize" or "commit" the changes. In
addition, any number of changes may be made to `m:wxAuiPaneInfo` structures (retrieved
with `getPane/2`), but to realize the changes, `update/1` must be called. This construction allows pane flicker
to be avoided by updating the whole layout at one time.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
