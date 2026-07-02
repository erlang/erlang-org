# `wxFrame`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxFrame.erl#L58)

A frame is a window whose size and position can (usually) be changed by the user.

It usually has thick borders and a title bar, and can optionally contain a menu bar,
toolbar and status bar. A frame can contain any window that is not a frame or dialog.

A frame that has a status bar and toolbar, created via the `createStatusBar/2` and `createToolBar/2` functions, manages these
windows and adjusts the value returned by `wxWindow:getClientSize/1` to reflect the remaining size available to
application windows.

Remark: An application should normally define an `m:wxCloseEvent` handler for the frame
to respond to system close events, for example so that related data and subwindows can be
cleaned up.

Default event processing

`m:wxFrame` processes the following events:

* `wxEVT_SIZE:` if the frame has exactly one child window, not counting the status and
toolbar, this child is resized to take the entire frame client area. If two or more
windows are present, they should be laid out explicitly either by manually handling `wxEVT_SIZE`
or using sizers;

* `wxEVT_MENU_HIGHLIGHT:` the default implementation displays the help string associated
with the selected item in the first pane of the status bar, if there is one.

## Styles

This class supports the following styles:

* wxDEFAULT_FRAME_STYLE: Defined as wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER |
wxSYSTEM_MENU | wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN.

* wxICONIZE: Display the frame iconized (minimized). Windows only.

* wxCAPTION: Puts a caption on the frame. Notice that this flag is required by
wxMINIMIZE_BOX, wxMAXIMIZE_BOX and wxCLOSE_BOX on most systems as the corresponding
buttons cannot be shown if the window has no title bar at all. I.e. if wxCAPTION is not
specified those styles would be simply ignored.

* wxMINIMIZE: Identical to wxICONIZE. Windows only.

* wxMINIMIZE_BOX: Displays a minimize box on the frame.

* wxMAXIMIZE: Displays the frame maximized. Windows and GTK+ only.

* wxMAXIMIZE_BOX: Displays a maximize box on the frame. Notice that under wxGTK
wxRESIZE_BORDER must be used as well or this style is ignored.

* wxCLOSE_BOX: Displays a close box on the frame.

* wxSTAY_ON_TOP: Stay on top of all other windows, see also wxFRAME_FLOAT_ON_PARENT.

* wxSYSTEM_MENU: Displays a system menu containing the list of various windows commands in
the window title bar. Unlike wxMINIMIZE_BOX, wxMAXIMIZE_BOX and wxCLOSE_BOX styles this
style can be used without wxCAPTION, at least under Windows, and makes the system menu
available without showing it on screen in this case. However it is recommended to only use
it together with wxCAPTION for consistent behaviour under all platforms.

* wxRESIZE_BORDER: Displays a resizable border around the window.

* wxFRAME_TOOL_WINDOW: Causes a frame with a small title bar to be created; the frame does
not appear in the taskbar under Windows or GTK+.

* wxFRAME_NO_TASKBAR: Creates an otherwise normal frame but it does not appear in the
taskbar under Windows or GTK+ (note that it will minimize to the desktop window under
Windows which may seem strange to the users and thus it might be better to use this style
only without wxMINIMIZE_BOX style). In wxGTK, the flag is respected only if the window
manager supports _NET_WM_STATE_SKIP_TASKBAR hint.

* wxFRAME_FLOAT_ON_PARENT: The frame will always be on top of its parent (unlike
wxSTAY_ON_TOP). A frame created with this style must have a non-NULL parent.

* wxFRAME_SHAPED: Windows with this style are allowed to have their shape changed with the `wxTopLevelWindow:setShape/2`
method. The default frame style is for normal, resizable frames. To create a frame which
cannot be resized by user, you may use the following combination of styles:

See also the overview_windowstyles.

## Extra Styles

This class supports the following extra styles:

* wxFRAME_EX_CONTEXTHELP: Under Windows, puts a query button on the caption. When pressed,
Windows will go into a context-sensitive help mode and wxWidgets will send a `wxEVT_HELP`
event if the user clicked on an application window. Note that this is an extended style
and must be set by calling SetExtraStyle before Create is called (two-step construction).
You cannot use this style together with wxMAXIMIZE_BOX or wxMINIMIZE_BOX, so you should
use wxDEFAULT_FRAME_STYLE ~ (wxMINIMIZE_BOX | wxMAXIMIZE_BOX) for the frames having this
style (the dialogs don't have a minimize or a maximize box by default)

* wxFRAME_EX_METAL: On macOS, frames with this style will be shown with a metallic look.
This is an extra style.

See:
* `m:wxMDIParentFrame`

* `m:wxMDIChildFrame`

* `m:wxMiniFrame`

* `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFrame](https://docs.wxwidgets.org/3.2/classwx_frame.html)

## Events

Event types emitted from this class:

* [`close_window`](`m:wxCloseEvent`)

* [`iconize`](`m:wxIconizeEvent`)

* [`menu_open`](`m:wxMenuEvent`)

* [`menu_close`](`m:wxMenuEvent`)

* [`menu_highlight`](`m:wxMenuEvent`)

# `wxFrame`

```elixir
-type wxFrame() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Title) -> boolean()
                when
                    This :: wxFrame(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata().
```

# `create`

```elixir
-spec create(This, Parent, Id, Title, [Option]) -> boolean()
                when
                    This :: wxFrame(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Title :: unicode:chardata(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Used in two-step frame construction.

See `new/4` for further details.

# `createStatusBar`

```elixir
-spec createStatusBar(This) -> wxStatusBar:wxStatusBar() when This :: wxFrame().
```

# `createStatusBar`

```elixir
-spec createStatusBar(This, [Option]) -> wxStatusBar:wxStatusBar()
                         when
                             This :: wxFrame(),
                             Option :: {number, integer()} | {style, integer()} | {id, integer()}.
```

Creates a status bar at the bottom of the frame.

Return: A pointer to the status bar if it was created successfully, NULL otherwise.

Remark: The width of the status bar is the whole width of the frame (adjusted
automatically when resizing), and the height and text size are chosen by the host
windowing system.

See:
* `setStatusText/3`

* `getStatusBar/1`

# `createToolBar`

```elixir
-spec createToolBar(This) -> wxToolBar:wxToolBar() when This :: wxFrame().
```

# `createToolBar`

```elixir
-spec createToolBar(This, [Option]) -> wxToolBar:wxToolBar()
                       when This :: wxFrame(), Option :: {style, integer()} | {id, integer()}.
```

Creates a toolbar at the top or left of the frame.

Return: A pointer to the toolbar if it was created successfully, NULL otherwise.

Remark: By default, the toolbar is an instance of `m:wxToolBar`. To use a different
class, override `OnCreateToolBar()` (not implemented in wx). When a toolbar has been
created with this function, or made known to the frame with `setToolBar/2`, the frame will manage the
toolbar position and adjust the return value from `wxWindow:getClientSize/1` to reflect the available space for
application windows. Under Pocket PC, you should always use this function for creating the
toolbar to be managed by the frame, so that wxWidgets can use a combined menubar and
toolbar. Where you manage your own toolbars, create a `m:wxToolBar` as usual.

See:
* `createStatusBar/2`

* `setToolBar/2`

* `getToolBar/1`

# `destroy`

```elixir
-spec destroy(This :: wxFrame()) -> ok.
```

Destroys the object

# `getClientAreaOrigin`

```elixir
-spec getClientAreaOrigin(This) -> {X :: integer(), Y :: integer()} when This :: wxFrame().
```

Returns the origin of the frame client area (in client coordinates).

It may be different from (0, 0) if the frame has a toolbar.

# `getMenuBar`

```elixir
-spec getMenuBar(This) -> wxMenuBar:wxMenuBar() when This :: wxFrame().
```

Returns a pointer to the menubar currently associated with the frame (if any).

See:
* `setMenuBar/2`

* `m:wxMenuBar`

* `m:wxMenu`

# `getStatusBar`

```elixir
-spec getStatusBar(This) -> wxStatusBar:wxStatusBar() when This :: wxFrame().
```

Returns a pointer to the status bar currently associated with the frame (if any).

See:
* `createStatusBar/2`

* `m:wxStatusBar`

# `getStatusBarPane`

```elixir
-spec getStatusBarPane(This) -> integer() when This :: wxFrame().
```

Returns the status bar pane used to display menu and toolbar help.

See: `setStatusBarPane/2`

# `getToolBar`

```elixir
-spec getToolBar(This) -> wxToolBar:wxToolBar() when This :: wxFrame().
```

Returns a pointer to the toolbar currently associated with the frame (if any).

See:
* `createToolBar/2`

* `m:wxToolBar`

* `setToolBar/2`

# `new`

```elixir
-spec new() -> wxFrame().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id, Title) -> wxFrame()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Title :: unicode:chardata().
```

# `new`

```elixir
-spec new(Parent, Id, Title, [Option]) -> wxFrame()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Title :: unicode:chardata(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor, creating the window.

Remark: For Motif, MWM (the Motif Window Manager) should be running for any window styles
to work (otherwise all styles take effect).

See: `create/5`

# `processCommand`

```elixir
-spec processCommand(This, Id) -> boolean() when This :: wxFrame(), Id :: integer().
```

Simulate a menu command.

# `sendSizeEvent`

```elixir
-spec sendSizeEvent(This) -> ok when This :: wxFrame().
```

# `sendSizeEvent`

```elixir
-spec sendSizeEvent(This, [Option]) -> ok when This :: wxFrame(), Option :: {flags, integer()}.
```

This function sends a dummy `m:wxSizeEvent` to the window allowing it to re-layout its
children positions.

It is sometimes useful to call this function after adding or deleting a children after
the frame creation or if a child size changes. Note that if the frame is using either
sizers or constraints for the children layout, it is enough to call `wxWindow:layout/1` directly and this
function should not be used in this case.

If `flags` includes `wxSEND_EVENT_POST` value, this function posts the event, i.e.
schedules it for later processing, instead of dispatching it directly. You can also use `PostSizeEvent()`
(not implemented in wx) as a more readable equivalent of calling this function with this flag.

# `setMenuBar`

```elixir
-spec setMenuBar(This, MenuBar) -> ok when This :: wxFrame(), MenuBar :: wxMenuBar:wxMenuBar().
```

Tells the frame to show the given menu bar.

Remark: If the frame is destroyed, the menu bar and its menus will be destroyed also, so
do not delete the menu bar explicitly (except by resetting the frame's menu bar to another
frame or NULL). Under Windows, a size event is generated, so be sure to initialize data
members properly before calling `setMenuBar/2`. Note that on some platforms, it is not possible to call
this function twice for the same frame object.

See:
* `getMenuBar/1`

* `m:wxMenuBar`

* `m:wxMenu`

# `setStatusBar`

```elixir
-spec setStatusBar(This, StatusBar) -> ok when This :: wxFrame(), StatusBar :: wxStatusBar:wxStatusBar().
```

Associates a status bar with the frame.

If `statusBar` is NULL, then the status bar, if present, is detached from the frame, but `not`
deleted.

See:
* `createStatusBar/2`

* `m:wxStatusBar`

* `getStatusBar/1`

# `setStatusBarPane`

```elixir
-spec setStatusBarPane(This, N) -> ok when This :: wxFrame(), N :: integer().
```

Set the status bar pane used to display menu and toolbar help.

Using -1 disables help display.

# `setStatusText`

```elixir
-spec setStatusText(This, Text) -> ok when This :: wxFrame(), Text :: unicode:chardata().
```

# `setStatusText`

```elixir
-spec setStatusText(This, Text, [Option]) -> ok
                       when This :: wxFrame(), Text :: unicode:chardata(), Option :: {number, integer()}.
```

Sets the status bar text and updates the status bar display.

This is a simple wrapper for `wxStatusBar:setStatusText/3` which doesn't do anything if the frame has no status bar,
i.e. `getStatusBar/1` returns NULL.

Remark: Use an empty string to clear the status bar.

See:
* `createStatusBar/2`

* `m:wxStatusBar`

# `setStatusWidths`

```elixir
-spec setStatusWidths(This, Widths_field) -> ok when This :: wxFrame(), Widths_field :: [integer()].
```

Sets the widths of the fields in the status bar.

Remark: The widths of the variable fields are calculated from the total width of all
fields, minus the sum of widths of the non-variable fields, divided by the number of
variable fields.

# `setToolBar`

```elixir
-spec setToolBar(This, ToolBar) -> ok when This :: wxFrame(), ToolBar :: wxToolBar:wxToolBar().
```

Associates a toolbar with the frame.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
