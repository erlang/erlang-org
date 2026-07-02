# `wxAuiPaneInfo`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxAuiPaneInfo.erl#L58)

`m:wxAuiPaneInfo` is part of the wxAUI class framework.

See also overview_aui.

`m:wxAuiPaneInfo` specifies all the parameters for a pane. These parameters specify where
the pane is on the screen, whether it is docked or floating, or hidden. In addition, these
parameters specify the pane's docked position, floating position, preferred size, minimum
size, caption text among many other parameters.

See:
* `m:wxAuiManager`

* `m:wxAuiDockArt`

wxWidgets docs: [wxAuiPaneInfo](https://docs.wxwidgets.org/3.2/classwx_aui_pane_info.html)

# `wxAuiPaneInfo`

```erlang
-type wxAuiPaneInfo() :: wx:wx_object().
```

# `bestSize`

```erlang
-spec bestSize(This, Size) -> wxAuiPaneInfo()
                  when This :: wxAuiPaneInfo(), Size :: {W :: integer(), H :: integer()}.
```

`bestSize/3` sets the ideal size for the pane.

The docking manager will attempt to use this size as much as possible when docking or
floating the pane.

# `bestSize`

```erlang
-spec bestSize(This, X, Y) -> wxAuiPaneInfo()
                  when This :: wxAuiPaneInfo(), X :: integer(), Y :: integer().
```

# `bottom`

```erlang
-spec bottom(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`bottom/1` sets the pane dock position to the bottom side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_BOTTOM).

# `bottomDockable`

```erlang
-spec bottomDockable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `bottomDockable`

```erlang
-spec bottomDockable(This, [Option]) -> wxAuiPaneInfo()
                        when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`bottomDockable/2` indicates whether a pane can be docked at the bottom of the frame.

# `caption`

```erlang
-spec caption(This, C) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), C :: unicode:chardata().
```

`caption/2` sets the caption of the pane.

# `captionVisible`

```erlang
-spec captionVisible(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `captionVisible`

```erlang
-spec captionVisible(This, [Option]) -> wxAuiPaneInfo()
                        when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

CaptionVisible indicates that a pane caption should be visible.

If false, no pane caption is drawn.

# `centre`

```erlang
-spec centre(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`Center()` (not implemented in wx) sets the pane dock position to the left side of the
frame.

The centre pane is the space in the middle after all border panes (left, top, right,
bottom) are subtracted from the layout. This is the same thing as calling
Direction(wxAUI_DOCK_CENTRE).

# `centrePane`

```erlang
-spec centrePane(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`centrePane/1` specifies that the pane should adopt the default center pane settings.

Centre panes usually do not have caption bars. This function provides an easy way of
preparing a pane to be displayed in the center dock position.

# `closeButton`

```erlang
-spec closeButton(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `closeButton`

```erlang
-spec closeButton(This, [Option]) -> wxAuiPaneInfo()
                     when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

`closeButton/2` indicates that a close button should be drawn for the pane.

# `defaultPane`

```erlang
-spec defaultPane(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`defaultPane/1` specifies that the pane should adopt the default pane settings.

# `destroy`

```erlang
-spec destroy(This :: wxAuiPaneInfo()) -> ok.
```

Destroys the object

# `destroyOnClose`

```erlang
-spec destroyOnClose(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `destroyOnClose`

```erlang
-spec destroyOnClose(This, [Option]) -> wxAuiPaneInfo()
                        when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`destroyOnClose/2` indicates whether a pane should be destroyed when it is closed.

Normally a pane is simply hidden when the close button is clicked. Setting DestroyOnClose
to true will cause the window to be destroyed when the user clicks the pane's close
button.

# `direction`

```erlang
-spec direction(This, Direction) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Direction :: integer().
```

`direction/2` determines the direction of the docked pane.

It is functionally the same as calling `left/1`, `right/1`, `top/1` or `bottom/1`, except that docking direction may be
specified programmatically via the parameter.

# `dock`

```erlang
-spec dock(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`dock/1` indicates that a pane should be docked.

It is the opposite of `float/1`.

# `dockable`

```erlang
-spec dockable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `dockable`

```erlang
-spec dockable(This, [Option]) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`dockable/2` specifies whether a frame can be docked or not.

It is the same as specifying
TopDockable(b).BottomDockable(b).LeftDockable(b).RightDockable(b).

# `fixed`

```erlang
-spec fixed(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`fixed/1` forces a pane to be fixed size so that it cannot be resized.

After calling `fixed/1`, `isFixed/1` will return true.

# `float`

```erlang
-spec float(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`float/1` indicates that a pane should be floated.

It is the opposite of `dock/1`.

# `floatable`

```erlang
-spec floatable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `floatable`

```erlang
-spec floatable(This, [Option]) -> wxAuiPaneInfo()
                   when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`floatable/2` sets whether the user will be able to undock a pane and turn it into a
floating window.

# `floatingPosition`

```erlang
-spec floatingPosition(This, Pos) -> wxAuiPaneInfo()
                          when This :: wxAuiPaneInfo(), Pos :: {X :: integer(), Y :: integer()}.
```

`floatingPosition/3` sets the position of the floating pane.

# `floatingPosition`

```erlang
-spec floatingPosition(This, X, Y) -> wxAuiPaneInfo()
                          when This :: wxAuiPaneInfo(), X :: integer(), Y :: integer().
```

# `floatingSize`

```erlang
-spec floatingSize(This, Size) -> wxAuiPaneInfo()
                      when This :: wxAuiPaneInfo(), Size :: {W :: integer(), H :: integer()}.
```

`floatingSize/3` sets the size of the floating pane.

# `floatingSize`

```erlang
-spec floatingSize(This, X, Y) -> wxAuiPaneInfo()
                      when This :: wxAuiPaneInfo(), X :: integer(), Y :: integer().
```

# `getDirection`

```erlang
-spec getDirection(This) -> integer() when This :: wxAuiPaneInfo().
```

# `getFloatingPosition`

```erlang
-spec getFloatingPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxAuiPaneInfo().
```

# `getFloatingSize`

```erlang
-spec getFloatingSize(This) -> {W :: integer(), H :: integer()} when This :: wxAuiPaneInfo().
```

# `getFrame`

```erlang
-spec getFrame(This) -> wxFrame:wxFrame() when This :: wxAuiPaneInfo().
```

# `getLayer`

```erlang
-spec getLayer(This) -> integer() when This :: wxAuiPaneInfo().
```

# `getPosition`

```erlang
-spec getPosition(This) -> integer() when This :: wxAuiPaneInfo().
```

# `getRow`

```erlang
-spec getRow(This) -> integer() when This :: wxAuiPaneInfo().
```

# `getWindow`

```erlang
-spec getWindow(This) -> wxWindow:wxWindow() when This :: wxAuiPaneInfo().
```

# `gripper`

```erlang
-spec gripper(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `gripper`

```erlang
-spec gripper(This, [Option]) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

`gripper/2` indicates that a gripper should be drawn for the pane.

# `gripperTop`

```erlang
-spec gripperTop(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `gripperTop`

```erlang
-spec gripperTop(This, [Option]) -> wxAuiPaneInfo()
                    when This :: wxAuiPaneInfo(), Option :: {attop, boolean()}.
```

`gripperTop/2` indicates that a gripper should be drawn at the top of the pane.

# `hasBorder`

```erlang
-spec hasBorder(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasBorder/1` returns true if the pane displays a border.

# `hasCaption`

```erlang
-spec hasCaption(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasCaption/1` returns true if the pane displays a caption.

# `hasCloseButton`

```erlang
-spec hasCloseButton(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasCloseButton/1` returns true if the pane displays a button to close the pane.

# `hasFlag`

```erlang
-spec hasFlag(This, Flag) -> boolean() when This :: wxAuiPaneInfo(), Flag :: integer().
```

`hasFlag/2` returns true if the property specified by flag is active for the pane.

# `hasGripper`

```erlang
-spec hasGripper(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasGripper/1` returns true if the pane displays a gripper.

# `hasGripperTop`

```erlang
-spec hasGripperTop(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasGripper/1` returns true if the pane displays a gripper at the top.

# `hasMaximizeButton`

```erlang
-spec hasMaximizeButton(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasMaximizeButton/1` returns true if the pane displays a button to maximize the pane.

# `hasMinimizeButton`

```erlang
-spec hasMinimizeButton(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasMinimizeButton/1` returns true if the pane displays a button to minimize the pane.

# `hasPinButton`

```erlang
-spec hasPinButton(This) -> boolean() when This :: wxAuiPaneInfo().
```

`hasPinButton/1` returns true if the pane displays a button to float the pane.

# `hide`

```erlang
-spec hide(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`hide/1` indicates that a pane should be hidden.

# `isBottomDockable`

```erlang
-spec isBottomDockable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isBottomDockable/1` returns true if the pane can be docked at the bottom of the managed
frame.

# `isDocked`

```erlang
-spec isDocked(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isDocked/1` returns true if the pane is currently docked.

# `isFixed`

```erlang
-spec isFixed(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isFixed/1` returns true if the pane cannot be resized.

# `isFloatable`

```erlang
-spec isFloatable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isFloatable/1` returns true if the pane can be undocked and displayed as a floating
window.

# `isFloating`

```erlang
-spec isFloating(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isFloating/1` returns true if the pane is floating.

# `isLeftDockable`

```erlang
-spec isLeftDockable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isLeftDockable/1` returns true if the pane can be docked on the left of the managed
frame.

# `isMovable`

```erlang
-spec isMovable(This) -> boolean() when This :: wxAuiPaneInfo().
```

IsMoveable() returns true if the docked frame can be undocked or moved to another dock
position.

# `isOk`

```erlang
-spec isOk(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isOk/1` returns true if the `m:wxAuiPaneInfo` structure is valid.

A pane structure is valid if it has an associated window.

# `isResizable`

```erlang
-spec isResizable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isResizable/1` returns true if the pane can be resized.

# `isRightDockable`

```erlang
-spec isRightDockable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isRightDockable/1` returns true if the pane can be docked on the right of the managed
frame.

# `isShown`

```erlang
-spec isShown(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isShown/1` returns true if the pane is currently shown.

# `isToolbar`

```erlang
-spec isToolbar(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isToolbar/1` returns true if the pane contains a toolbar.

# `isTopDockable`

```erlang
-spec isTopDockable(This) -> boolean() when This :: wxAuiPaneInfo().
```

`isTopDockable/1` returns true if the pane can be docked at the top of the managed frame.

# `layer`

```erlang
-spec layer(This, Layer) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Layer :: integer().
```

`layer/2` determines the layer of the docked pane.

The dock layer is similar to an onion, the inner-most layer being layer 0. Each shell
moving in the outward direction has a higher layer number. This allows for more complex
docking layout formation.

# `left`

```erlang
-spec left(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`left/1` sets the pane dock position to the left side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_LEFT).

# `leftDockable`

```erlang
-spec leftDockable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `leftDockable`

```erlang
-spec leftDockable(This, [Option]) -> wxAuiPaneInfo()
                      when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`leftDockable/2` indicates whether a pane can be docked on the left of the frame.

# `maximizeButton`

```erlang
-spec maximizeButton(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `maximizeButton`

```erlang
-spec maximizeButton(This, [Option]) -> wxAuiPaneInfo()
                        when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

`maximizeButton/2` indicates that a maximize button should be drawn for the pane.

# `maxSize`

```erlang
-spec maxSize(This, Size) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), Size :: {W :: integer(), H :: integer()}.
```

`maxSize/3` sets the maximum size of the pane.

# `maxSize`

```erlang
-spec maxSize(This, X, Y) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), X :: integer(), Y :: integer().
```

# `minimizeButton`

```erlang
-spec minimizeButton(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `minimizeButton`

```erlang
-spec minimizeButton(This, [Option]) -> wxAuiPaneInfo()
                        when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

`minimizeButton/2` indicates that a minimize button should be drawn for the pane.

# `minSize`

```erlang
-spec minSize(This, Size) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), Size :: {W :: integer(), H :: integer()}.
```

`minSize/3` sets the minimum size of the pane.

Please note that this is only partially supported as of this writing.

# `minSize`

```erlang
-spec minSize(This, X, Y) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), X :: integer(), Y :: integer().
```

# `movable`

```erlang
-spec movable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `movable`

```erlang
-spec movable(This, [Option]) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

Movable indicates whether a frame can be moved.

# `name`

```erlang
-spec name(This, N) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), N :: unicode:chardata().
```

`name/2` sets the name of the pane so it can be referenced in lookup functions.

If a name is not specified by the user, a random name is assigned to the pane when it is
added to the manager.

# `new`

```erlang
-spec new() -> wxAuiPaneInfo().
```

# `new`

```erlang
-spec new(C) -> wxAuiPaneInfo() when C :: wxAuiPaneInfo().
```

Copy constructor.

# `paneBorder`

```erlang
-spec paneBorder(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `paneBorder`

```erlang
-spec paneBorder(This, [Option]) -> wxAuiPaneInfo()
                    when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

PaneBorder indicates that a border should be drawn for the pane.

# `pinButton`

```erlang
-spec pinButton(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `pinButton`

```erlang
-spec pinButton(This, [Option]) -> wxAuiPaneInfo()
                   when This :: wxAuiPaneInfo(), Option :: {visible, boolean()}.
```

`pinButton/2` indicates that a pin button should be drawn for the pane.

# `position`

```erlang
-spec position(This, Pos) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Pos :: integer().
```

`position/2` determines the position of the docked pane.

# `resizable`

```erlang
-spec resizable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `resizable`

```erlang
-spec resizable(This, [Option]) -> wxAuiPaneInfo()
                   when This :: wxAuiPaneInfo(), Option :: {resizable, boolean()}.
```

`resizable/2` allows a pane to be resized if the parameter is true, and forces it to be a
fixed size if the parameter is false.

This is simply an antonym for `fixed/1`.

# `right`

```erlang
-spec right(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`right/1` sets the pane dock position to the right side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_RIGHT).

# `rightDockable`

```erlang
-spec rightDockable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `rightDockable`

```erlang
-spec rightDockable(This, [Option]) -> wxAuiPaneInfo()
                       when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`rightDockable/2` indicates whether a pane can be docked on the right of the frame.

# `row`

```erlang
-spec row(This, Row) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Row :: integer().
```

`row/2` determines the row of the docked pane.

# `safeSet`

```erlang
-spec safeSet(This, Source) -> ok when This :: wxAuiPaneInfo(), Source :: wxAuiPaneInfo().
```

Write the safe parts of a PaneInfo object "source" into "this".

"Safe parts" are all non-UI elements (e.g. all layout determining parameters like the
size, position etc.). "Unsafe parts" (pointers to button, frame and window) are not
modified by this write operation.

Remark: This method is used when loading perspectives.

# `setFlag`

```erlang
-spec setFlag(This, Flag, Option_state) -> wxAuiPaneInfo()
                 when This :: wxAuiPaneInfo(), Flag :: integer(), Option_state :: boolean().
```

`setFlag/3` turns the property given by flag on or off with the option_state parameter.

# `show`

```erlang
-spec show(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `show`

```erlang
-spec show(This, [Option]) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), Option :: {show, boolean()}.
```

`show/2` indicates that a pane should be shown.

# `toolbarPane`

```erlang
-spec toolbarPane(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`toolbarPane/1` specifies that the pane should adopt the default toolbar pane settings.

# `top`

```erlang
-spec top(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

`top/1` sets the pane dock position to the top of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_TOP).

# `topDockable`

```erlang
-spec topDockable(This) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo().
```

# `topDockable`

```erlang
-spec topDockable(This, [Option]) -> wxAuiPaneInfo()
                     when This :: wxAuiPaneInfo(), Option :: {b, boolean()}.
```

`topDockable/2` indicates whether a pane can be docked at the top of the frame.

# `window`

```erlang
-spec window(This, W) -> wxAuiPaneInfo() when This :: wxAuiPaneInfo(), W :: wxWindow:wxWindow().
```

`window/2` assigns the window pointer that the `m:wxAuiPaneInfo` should use.

This normally does not need to be specified, as the window pointer is automatically
assigned to the `m:wxAuiPaneInfo` structure as soon as it is added to the manager.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
