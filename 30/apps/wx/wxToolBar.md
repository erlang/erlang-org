# `wxToolBar`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxToolBar.erl#L58)

A toolbar is a bar of buttons and/or other controls usually placed below the menu bar in
a `m:wxFrame`.

You may create a toolbar that is managed by a frame calling `wxFrame:createToolBar/2`. Under Pocket PC, you should
always use this function for creating the toolbar to be managed by the frame, so that
wxWidgets can use a combined menubar and toolbar. Where you manage your own toolbars,
create `m:wxToolBar` as usual.

There are several different types of tools you can add to a toolbar. These types are
controlled by the ?wxItemKind enumeration.

Note that many methods in `m:wxToolBar` such as `addTool/6` return a `wxToolBarToolBase*` object.
This should be regarded as an opaque handle representing the newly added toolbar item,
providing access to its id and position within the toolbar. Changes to the item's state
should be made through calls to `m:wxToolBar` methods, for example `enableTool/3`. Calls to `wxToolBarToolBase`
(not implemented in wx) methods (undocumented by purpose) will not change the visible
state of the item within the tool bar.

After you have added all the tools you need, you must call `realize/1` to effectively construct and
display the toolbar.

`wxMSW note`: Note that under wxMSW toolbar paints tools to reflect system-wide colours.
If you use more than 16 colours in your tool bitmaps, you may wish to suppress this
behaviour, otherwise system colours in your bitmaps will inadvertently be mapped to system
colours. To do this, set the msw.remap system option before creating the toolbar: If you
wish to use 32-bit images (which include an alpha channel for transparency) use: Then
colour remapping is switched off, and a transparent background used. But only use this
option under Windows XP with true colour:

## Styles

This class supports the following styles:

* wxTB_FLAT: Gives the toolbar a flat look (Windows and GTK only).

* wxTB_DOCKABLE: Makes the toolbar floatable and dockable (GTK only).

* wxTB_HORIZONTAL: Specifies horizontal layout (default).

* wxTB_VERTICAL: Specifies vertical layout.

* wxTB_TEXT: Shows the text in the toolbar buttons; by default only icons are shown.

* wxTB_NOICONS: Specifies no icons in the toolbar buttons; by default they are shown.

* wxTB_NODIVIDER: Specifies no divider (border) above the toolbar (Windows only)

* wxTB_NOALIGN: Specifies no alignment with the parent window (Windows only, not very
useful).

* wxTB_HORZ_LAYOUT: Shows the text and the icons alongside, not vertically stacked (Windows
and GTK 2 only). This style must be used with `wxTB_TEXT`.

* wxTB_HORZ_TEXT: Combination of `wxTB_HORZ_LAYOUT` and `wxTB_TEXT`.

* wxTB_NO_TOOLTIPS: Don't show the short help tooltips for the tools when the mouse hovers
over them.

* wxTB_BOTTOM: Align the toolbar at the bottom of parent window.

* wxTB_RIGHT: Align the toolbar at the right side of parent window.

* wxTB_DEFAULT_STYLE: Combination of `wxTB_HORIZONTAL` and `wxTB_FLAT`. This style is new
since wxWidgets 2.9.5. See also overview_windowstyles. Note that the wxMSW native toolbar
ignores `wxTB_NOICONS` style. Also, toggling the `wxTB_TEXT` works only if the style was
initially on.

See: [Overview toolbar](https://docs.wxwidgets.org/3.2/overview_toolbar.html#overview_toolbar)

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxToolBar](https://docs.wxwidgets.org/3.2/classwx_tool_bar.html)

## Events

Event types emitted from this class:

* [`command_tool_rclicked`](`m:wxCommandEvent`)

* [`command_tool_enter`](`m:wxCommandEvent`)

* [`tool_dropdown`](`m:wxCommandEvent`)

# `wxToolBar`

```erlang
-type wxToolBar() :: wx:wx_object().
```

# `addCheckTool`

```erlang
-spec addCheckTool(This, ToolId, Label, Bitmap1) -> wx:wx_object()
                      when
                          This :: wxToolBar(),
                          ToolId :: integer(),
                          Label :: unicode:chardata(),
                          Bitmap1 :: wxBitmap:wxBitmap().
```

# `addCheckTool`

```erlang
-spec addCheckTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object()
                      when
                          This :: wxToolBar(),
                          ToolId :: integer(),
                          Label :: unicode:chardata(),
                          Bitmap1 :: wxBitmap:wxBitmap(),
                          Option ::
                              {bmpDisabled, wxBitmap:wxBitmap()} |
                              {shortHelp, unicode:chardata()} |
                              {longHelp, unicode:chardata()} |
                              {data, wx:wx_object()}.
```

Adds a new check (or toggle) tool to the toolbar.

The parameters are the same as in `addTool/6`.

See: `addTool/6`

# `addControl`

```erlang
-spec addControl(This, Control) -> wx:wx_object()
                    when This :: wxToolBar(), Control :: wxControl:wxControl().
```

# `addControl`

```erlang
-spec addControl(This, Control, [Option]) -> wx:wx_object()
                    when
                        This :: wxToolBar(),
                        Control :: wxControl:wxControl(),
                        Option :: {label, unicode:chardata()}.
```

Adds any control to the toolbar, typically e.g. a `m:wxComboBox`.

Remark: wxMac: labels are only displayed if wxWidgets is built with `wxMAC_USE_NATIVE_TOOLBAR`
set to 1

# `addRadioTool`

```erlang
-spec addRadioTool(This, ToolId, Label, Bitmap1) -> wx:wx_object()
                      when
                          This :: wxToolBar(),
                          ToolId :: integer(),
                          Label :: unicode:chardata(),
                          Bitmap1 :: wxBitmap:wxBitmap().
```

# `addRadioTool`

```erlang
-spec addRadioTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object()
                      when
                          This :: wxToolBar(),
                          ToolId :: integer(),
                          Label :: unicode:chardata(),
                          Bitmap1 :: wxBitmap:wxBitmap(),
                          Option ::
                              {bmpDisabled, wxBitmap:wxBitmap()} |
                              {shortHelp, unicode:chardata()} |
                              {longHelp, unicode:chardata()} |
                              {data, wx:wx_object()}.
```

Adds a new radio tool to the toolbar.

Consecutive radio tools form a radio group such that exactly one button in the group is
pressed at any moment, in other words whenever a button in the group is pressed the
previously pressed button is automatically released. You should avoid having the radio
groups of only one element as it would be impossible for the user to use such button.

By default, the first button in the radio group is initially pressed, the others are not.

See: `addTool/6`

# `addSeparator`

```erlang
-spec addSeparator(This) -> wx:wx_object() when This :: wxToolBar().
```

Adds a separator for spacing groups of tools.

Notice that the separator uses the look appropriate for the current platform so it can be
a vertical line (MSW, some versions of GTK) or just an empty space or something else.

See:
* `addTool/6`

* `setToolSeparation/2`

* `addStretchableSpace/1`

# `addStretchableSpace`

```erlang
-spec addStretchableSpace(This) -> wx:wx_object() when This :: wxToolBar().
```

Adds a stretchable space to the toolbar.

Any space not taken up by the fixed items (all items except for stretchable spaces) is
distributed in equal measure between the stretchable spaces in the toolbar. The most
common use for this method is to add a single stretchable space before the items which
should be right-aligned in the toolbar, but more exotic possibilities are possible, e.g. a
stretchable space may be added in the beginning and the end of the toolbar to centre all
toolbar items.

See:
* `addTool/6`

* `addSeparator/1`

* `insertStretchableSpace/2`

Since: 2.9.1

# `addTool`

```erlang
-spec addTool(This, Tool) -> wx:wx_object() when This :: wxToolBar(), Tool :: wx:wx_object().
```

Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`

# `addTool`

```erlang
-spec addTool(This, ToolId, Label, Bitmap) -> wx:wx_object()
                 when
                     This :: wxToolBar(),
                     ToolId :: integer(),
                     Label :: unicode:chardata(),
                     Bitmap :: wxBitmap:wxBitmap().
```

# `addTool`

```erlang
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled) -> wx:wx_object()
                 when
                     This :: wxToolBar(),
                     ToolId :: integer(),
                     Label :: unicode:chardata(),
                     Bitmap :: wxBitmap:wxBitmap(),
                     BmpDisabled :: wxBitmap:wxBitmap();
             (This, ToolId, Label, Bitmap, [Option]) -> wx:wx_object()
                 when
                     This :: wxToolBar(),
                     ToolId :: integer(),
                     Label :: unicode:chardata(),
                     Bitmap :: wxBitmap:wxBitmap(),
                     Option :: {shortHelp, unicode:chardata()} | {kind, wx:wx_enum()}.
```

Adds a tool to the toolbar.

This most commonly used version has fewer parameters than the full version below which
specifies the more rarely used button features.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`

# `addTool`

```erlang
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled, [Option]) -> wx:wx_object()
                 when
                     This :: wxToolBar(),
                     ToolId :: integer(),
                     Label :: unicode:chardata(),
                     Bitmap :: wxBitmap:wxBitmap(),
                     BmpDisabled :: wxBitmap:wxBitmap(),
                     Option ::
                         {kind, wx:wx_enum()} |
                         {shortHelp, unicode:chardata()} |
                         {longHelp, unicode:chardata()} |
                         {data, wx:wx_object()}.
```

Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`

# `deleteTool`

```erlang
-spec deleteTool(This, ToolId) -> boolean() when This :: wxToolBar(), ToolId :: integer().
```

Removes the specified tool from the toolbar and deletes it.

If you don't want to delete the tool, but just to remove it from the toolbar (to possibly
add it back later), you may use `removeTool/2` instead.

Note: It is unnecessary to call `realize/1` for the change to take place, it will happen immediately.

Return: true if the tool was deleted, false otherwise.

See: `deleteToolByPos/2`

# `deleteToolByPos`

```erlang
-spec deleteToolByPos(This, Pos) -> boolean() when This :: wxToolBar(), Pos :: integer().
```

This function behaves like `deleteTool/2` but it deletes the tool at the specified
position and not the one with the given id.

# `enableTool`

```erlang
-spec enableTool(This, ToolId, Enable) -> ok
                    when This :: wxToolBar(), ToolId :: integer(), Enable :: boolean().
```

Enables or disables the tool.

Remark: Some implementations will change the visible state of the tool to indicate that
it is disabled.

See:
* `getToolEnabled/2`

* `toggleTool/3`

# `findById`

```erlang
-spec findById(This, Id) -> wx:wx_object() when This :: wxToolBar(), Id :: integer().
```

Returns a pointer to the tool identified by `id` or NULL if no corresponding tool is
found.

# `findControl`

```erlang
-spec findControl(This, Id) -> wxControl:wxControl() when This :: wxToolBar(), Id :: integer().
```

Returns a pointer to the control identified by `id` or NULL if no corresponding control
is found.

# `findToolForPosition`

```erlang
-spec findToolForPosition(This, X, Y) -> wx:wx_object()
                             when This :: wxToolBar(), X :: integer(), Y :: integer().
```

Finds a tool for the given mouse position.

Return: A pointer to a tool if a tool is found, or NULL otherwise.

Remark: Currently not implemented in wxGTK (always returns NULL there).

# `getMargins`

```erlang
-spec getMargins(This) -> {W :: integer(), H :: integer()} when This :: wxToolBar().
```

Returns the left/right and top/bottom margins, which are also used for inter-toolspacing.

See: `setMargins/3`

# `getToolBitmapSize`

```erlang
-spec getToolBitmapSize(This) -> {W :: integer(), H :: integer()} when This :: wxToolBar().
```

Returns the size of bitmap that the toolbar expects to have.

The default bitmap size is platform-dependent: for example, it is 16*15 for MSW and 24*24
for GTK. This size does `not` necessarily indicate the best size to use for the toolbars
on the given platform, for this you should use `wxArtProvider::GetNativeSizeHint(wxART_TOOLBAR)`
but in any case, as the bitmap size is deduced automatically from the size of the bitmaps
associated with the tools added to the toolbar, it is usually unnecessary to call `setToolBitmapSize/2` explicitly.

Remark: Note that this is the size of the bitmap you pass to `addTool/6`, and not the eventual size
of the tool button.

See:
* `setToolBitmapSize/2`

* `getToolSize/1`

# `getToolEnabled`

```erlang
-spec getToolEnabled(This, ToolId) -> boolean() when This :: wxToolBar(), ToolId :: integer().
```

Called to determine whether a tool is enabled (responds to user input).

Return: true if the tool is enabled, false otherwise.

See: `enableTool/3`

# `getToolLongHelp`

```erlang
-spec getToolLongHelp(This, ToolId) -> unicode:charlist() when This :: wxToolBar(), ToolId :: integer().
```

Returns the long help for the given tool.

See:
* `setToolLongHelp/3`

* `setToolShortHelp/3`

# `getToolPacking`

```erlang
-spec getToolPacking(This) -> integer() when This :: wxToolBar().
```

Returns the value used for packing tools.

See: `setToolPacking/2`

# `getToolPos`

```erlang
-spec getToolPos(This, ToolId) -> integer() when This :: wxToolBar(), ToolId :: integer().
```

Returns the tool position in the toolbar, or `wxNOT_FOUND` if the tool is not found.

# `getToolSeparation`

```erlang
-spec getToolSeparation(This) -> integer() when This :: wxToolBar().
```

Returns the default separator size.

See: `setToolSeparation/2`

# `getToolShortHelp`

```erlang
-spec getToolShortHelp(This, ToolId) -> unicode:charlist() when This :: wxToolBar(), ToolId :: integer().
```

Returns the short help for the given tool.

See:
* `getToolLongHelp/2`

* `setToolShortHelp/3`

# `getToolSize`

```erlang
-spec getToolSize(This) -> {W :: integer(), H :: integer()} when This :: wxToolBar().
```

Returns the size of a whole button, which is usually larger than a tool bitmap because of
added 3D effects.

See:
* `setToolBitmapSize/2`

* `getToolBitmapSize/1`

# `getToolState`

```erlang
-spec getToolState(This, ToolId) -> boolean() when This :: wxToolBar(), ToolId :: integer().
```

Gets the on/off state of a toggle tool.

Return: true if the tool is toggled on, false otherwise.

See: `toggleTool/3`

# `insertControl`

```erlang
-spec insertControl(This, Pos, Control) -> wx:wx_object()
                       when This :: wxToolBar(), Pos :: integer(), Control :: wxControl:wxControl().
```

# `insertControl`

```erlang
-spec insertControl(This, Pos, Control, [Option]) -> wx:wx_object()
                       when
                           This :: wxToolBar(),
                           Pos :: integer(),
                           Control :: wxControl:wxControl(),
                           Option :: {label, unicode:chardata()}.
```

Inserts the control into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addControl/3`

* `insertTool/6`

# `insertSeparator`

```erlang
-spec insertSeparator(This, Pos) -> wx:wx_object() when This :: wxToolBar(), Pos :: integer().
```

Inserts the separator into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addSeparator/1`

* `insertTool/6`

# `insertStretchableSpace`

```erlang
-spec insertStretchableSpace(This, Pos) -> wx:wx_object() when This :: wxToolBar(), Pos :: integer().
```

Inserts a stretchable space at the given position.

See `addStretchableSpace/1` for details about stretchable spaces.

See:
* `insertTool/6`

* `insertSeparator/2`

Since: 2.9.1

# `insertTool`

```erlang
-spec insertTool(This, Pos, Tool) -> wx:wx_object()
                    when This :: wxToolBar(), Pos :: integer(), Tool :: wx:wx_object().
```

# `insertTool`

```erlang
-spec insertTool(This, Pos, ToolId, Label, Bitmap) -> wx:wx_object()
                    when
                        This :: wxToolBar(),
                        Pos :: integer(),
                        ToolId :: integer(),
                        Label :: unicode:chardata(),
                        Bitmap :: wxBitmap:wxBitmap().
```

# `insertTool`

```erlang
-spec insertTool(This, Pos, ToolId, Label, Bitmap, [Option]) -> wx:wx_object()
                    when
                        This :: wxToolBar(),
                        Pos :: integer(),
                        ToolId :: integer(),
                        Label :: unicode:chardata(),
                        Bitmap :: wxBitmap:wxBitmap(),
                        Option ::
                            {bmpDisabled, wxBitmap:wxBitmap()} |
                            {kind, wx:wx_enum()} |
                            {shortHelp, unicode:chardata()} |
                            {longHelp, unicode:chardata()} |
                            {clientData, wx:wx_object()}.
```

Inserts the tool with the specified attributes into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addTool/6`

* `insertControl/4`

* `insertSeparator/2`

Return: The newly inserted tool or NULL on failure. Notice that with the overload taking `tool`
parameter the caller is responsible for deleting the tool in the latter case.

# `realize`

```erlang
-spec realize(This) -> boolean() when This :: wxToolBar().
```

This function should be called after you have added tools.

# `removeTool`

```erlang
-spec removeTool(This, Id) -> wx:wx_object() when This :: wxToolBar(), Id :: integer().
```

Removes the given tool from the toolbar but doesn't delete it.

This allows inserting/adding this tool back to this (or another) toolbar later.

Note: It is unnecessary to call `realize/1` for the change to take place, it will happen immediately.

See: `deleteTool/2`

# `setMargins`

```erlang
-spec setMargins(This, X, Y) -> ok when This :: wxToolBar(), X :: integer(), Y :: integer().
```

Set the values to be used as margins for the toolbar.

Remark: This must be called before the tools are added if absolute positioning is to be
used, and the default (zero-size) margins are to be overridden.

See: `getMargins/1`

# `setToolBitmapSize`

```erlang
-spec setToolBitmapSize(This, Size) -> ok
                           when This :: wxToolBar(), Size :: {W :: integer(), H :: integer()}.
```

Sets the default size of each tool bitmap.

The default bitmap size is 16 by 15 pixels.

Remark: This should be called to tell the toolbar what the tool bitmap size is. Call it
before you add tools.

See:
* `getToolBitmapSize/1`

* `getToolSize/1`

# `setToolLongHelp`

```erlang
-spec setToolLongHelp(This, ToolId, HelpString) -> ok
                         when This :: wxToolBar(), ToolId :: integer(), HelpString :: unicode:chardata().
```

Sets the long help for the given tool.

Remark: You might use the long help for displaying the tool purpose on the status line.

See:
* `getToolLongHelp/2`

* `setToolShortHelp/3`

# `setToolPacking`

```erlang
-spec setToolPacking(This, Packing) -> ok when This :: wxToolBar(), Packing :: integer().
```

Sets the value used for spacing tools.

The default value is 1.

Remark: The packing is used for spacing in the vertical direction if the toolbar is
horizontal, and for spacing in the horizontal direction if the toolbar is vertical.

See: `getToolPacking/1`

# `setToolSeparation`

```erlang
-spec setToolSeparation(This, Separation) -> ok when This :: wxToolBar(), Separation :: integer().
```

Sets the default separator size.

The default value is 5.

See: `addSeparator/1`

# `setToolShortHelp`

```erlang
-spec setToolShortHelp(This, ToolId, HelpString) -> ok
                          when
                              This :: wxToolBar(), ToolId :: integer(), HelpString :: unicode:chardata().
```

Sets the short help for the given tool.

Remark: An application might use short help for identifying the tool purpose in a tooltip.

See:
* `getToolShortHelp/2`

* `setToolLongHelp/3`

# `toggleTool`

```erlang
-spec toggleTool(This, ToolId, Toggle) -> ok
                    when This :: wxToolBar(), ToolId :: integer(), Toggle :: boolean().
```

Toggles a tool on or off.

This does not cause any event to get emitted.

Remark: Only applies to a tool that has been specified as a toggle tool.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
