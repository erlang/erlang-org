# `wxWindow`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxWindow.erl#L58)

`m:wxWindow` is the base class for all windows and represents any visible object on
screen.

All controls, top level windows and so on are windows. Sizers and device contexts are
not, however, as they don't appear on screen themselves.

Please note that all children of the window will be deleted automatically by the
destructor before the window itself is deleted which means that you don't have to worry
about deleting them manually. Please see the window deletion overview for more information.

Also note that in this, and many others, wxWidgets classes some `GetXXX()` methods may be
overloaded (as, for example, `getSize/1` or `getClientSize/1`). In this case, the overloads are non-virtual because
having multiple virtual functions with the same name results in a virtual function name
hiding at the derived class level (in English, this means that the derived class has to
override all overloaded variants if it overrides any of them). To allow overriding them in
the derived class, wxWidgets uses a unique protected virtual `DoGetXXX()` method and all `GetXXX()`
ones are forwarded to it, so overriding the former changes the behaviour of the latter.

## Styles

This class supports the following styles:

* wxBORDER_DEFAULT: The window class will decide the kind of border to show, if any.

* wxBORDER_SIMPLE: Displays a thin border around the window. wxSIMPLE_BORDER is the old
name for this style.

* wxBORDER_SUNKEN: Displays a sunken border. wxSUNKEN_BORDER is the old name for this
style.

* wxBORDER_RAISED: Displays a raised border. wxRAISED_BORDER is the old name for this
style.

* wxBORDER_STATIC: Displays a border suitable for a static control. wxSTATIC_BORDER is the
old name for this style. Windows only.

* wxBORDER_THEME: Displays a native border suitable for a control, on the current platform.
On Windows, this will be a themed border; on most other platforms a sunken border will be
used. For more information for themed borders on Windows, please see Themed borders on
Windows.

* wxBORDER_NONE: Displays no border, overriding the default border style for the window.
wxNO_BORDER is the old name for this style.

* wxBORDER_DOUBLE: This style is obsolete and should not be used.

* wxTRANSPARENT_WINDOW: The window is transparent, that is, it will not receive paint
events. Windows only.

* wxTAB_TRAVERSAL: This style is used by wxWidgets for the windows supporting TAB
navigation among their children, such as `m:wxDialog` and `m:wxPanel`. It should almost
never be used in the application code.

* wxWANTS_CHARS: Use this to indicate that the window wants to get all char/key events for
all keys - even for keys like TAB or ENTER which are usually used for dialog navigation
and which wouldn't be generated without this style. If you need to use this style in order
to get the arrows or etc., but would still like to have normal keyboard navigation take
place, you should call Navigate in response to the key events for Tab and Shift-Tab.

* wxNO_FULL_REPAINT_ON_RESIZE: On Windows, this style used to disable repainting the window
completely when its size is changed. Since this behaviour is now the default, the style is
now obsolete and no longer has an effect.

* wxVSCROLL: Use this style to enable a vertical scrollbar. Notice that this style cannot
be used with native controls which don't support scrollbars nor with top-level windows in
most ports.

* wxHSCROLL: Use this style to enable a horizontal scrollbar. The same limitations as for
wxVSCROLL apply to this style.

* wxALWAYS_SHOW_SB: If a window has scrollbars, disable them instead of hiding them when
they are not needed (i.e. when the size of the window is big enough to not require the
scrollbars to navigate it). This style is currently implemented for wxMSW, wxGTK and
wxUniversal and does nothing on the other platforms.

* wxCLIP_CHILDREN: Use this style to eliminate flicker caused by the background being
repainted, then children being painted over them. Windows only.

* wxFULL_REPAINT_ON_RESIZE: Use this style to force a complete redraw of the window
whenever it is resized instead of redrawing just the part of the window affected by
resizing. Note that this was the behaviour by default before 2.5.1 release and that if you
experience redraw problems with code which previously used to work you may want to try
this. Currently this style applies on GTK+ 2 and Windows only, and full repainting is
always done on other platforms.

## Extra Styles

This class supports the following extra styles:

* wxWS_EX_BLOCK_EVENTS: wxCommandEvents and the objects of the derived classes are
forwarded to the parent window and so on recursively by default. Using this flag for the
given window allows blocking this propagation at this window, i.e. prevent the events from
being propagated further upwards. Dialogs have this flag on by default for the reasons
explained in the overview_events.

* wxWS_EX_TRANSIENT: Don't use this window as an implicit parent for the other windows:
this must be used with transient windows as otherwise there is the risk of creating a
dialog/frame with this window as a parent, which would lead to a crash if the parent were
destroyed before the child.

* wxWS_EX_CONTEXTHELP: Under Windows, puts a query button on the caption. When pressed,
Windows will go into a context-sensitive help mode and wxWidgets will send a `wxEVT_HELP`
event if the user clicked on an application window. This style cannot be used (because of
the underlying native behaviour) together with `wxMAXIMIZE_BOX` or `wxMINIMIZE_BOX`, so
these two styles are automatically turned off if this one is used.

* wxWS_EX_PROCESS_IDLE: This window should always process idle events, even if the mode set
by `wxIdleEvent:setMode/1` is `wxIDLE_PROCESS_SPECIFIED`.

* wxWS_EX_PROCESS_UI_UPDATES: This window should always process UI update events, even if
the mode set by `wxUpdateUIEvent:setMode/1` is `wxUPDATE_UI_PROCESS_SPECIFIED`.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxWindow](https://docs.wxwidgets.org/3.2/classwx_window.html)

## Events

Event types emitted from this class:

* [`activate`](`m:wxActivateEvent`)

* [`child_focus`](`m:wxChildFocusEvent`)

* [`context_menu`](`m:wxContextMenuEvent`)

* [`help`](`m:wxHelpEvent`)

* [`drop_files`](`m:wxDropFilesEvent`)

* [`erase_background`](`m:wxEraseEvent`)

* [`set_focus`](`m:wxFocusEvent`)

* [`kill_focus`](`m:wxFocusEvent`)

* [`idle`](`m:wxIdleEvent`)

* [`joy_button_down`](`m:wxJoystickEvent`)

* [`joy_button_up`](`m:wxJoystickEvent`)

* [`joy_move`](`m:wxJoystickEvent`)

* [`joy_zmove`](`m:wxJoystickEvent`)

* [`key_down`](`m:wxKeyEvent`)

* [`key_up`](`m:wxKeyEvent`)

* [`char`](`m:wxKeyEvent`)

* [`char_hook`](`m:wxKeyEvent`)

* [`mouse_capture_lost`](`m:wxMouseCaptureLostEvent`)

* [`mouse_capture_changed`](`m:wxMouseCaptureChangedEvent`)

* [`left_down`](`m:wxMouseEvent`)

* [`left_up`](`m:wxMouseEvent`)

* [`middle_down`](`m:wxMouseEvent`)

* [`middle_up`](`m:wxMouseEvent`)

* [`right_down`](`m:wxMouseEvent`)

* [`right_up`](`m:wxMouseEvent`)

* [`motion`](`m:wxMouseEvent`)

* [`enter_window`](`m:wxMouseEvent`)

* [`leave_window`](`m:wxMouseEvent`)

* [`left_dclick`](`m:wxMouseEvent`)

* [`middle_dclick`](`m:wxMouseEvent`)

* [`right_dclick`](`m:wxMouseEvent`)

* [`mousewheel`](`m:wxMouseEvent`)

* [`aux1_down`](`m:wxMouseEvent`)

* [`aux1_up`](`m:wxMouseEvent`)

* [`aux1_dclick`](`m:wxMouseEvent`)

* [`aux2_down`](`m:wxMouseEvent`)

* [`aux2_up`](`m:wxMouseEvent`)

* [`aux2_dclick`](`m:wxMouseEvent`)

* [`paint`](`m:wxPaintEvent`)

* [`scrollwin_top`](`m:wxScrollWinEvent`)

* [`scrollwin_bottom`](`m:wxScrollWinEvent`)

* [`scrollwin_lineup`](`m:wxScrollWinEvent`)

* [`scrollwin_linedown`](`m:wxScrollWinEvent`)

* [`scrollwin_pageup`](`m:wxScrollWinEvent`)

* [`scrollwin_pagedown`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbtrack`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbrelease`](`m:wxScrollWinEvent`)

* [`set_cursor`](`m:wxSetCursorEvent`)

* [`size`](`m:wxSizeEvent`)

* [`sys_colour_changed`](`m:wxSysColourChangedEvent`)

# `wxWindow`

```erlang
-type wxWindow() :: wx:wx_object().
```

# `cacheBestSize`

```erlang
-spec cacheBestSize(This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

Sets the cached best size value.

See: `getBestSize/1`

# `canSetTransparent`

```erlang
-spec canSetTransparent(This) -> boolean() when This :: wxWindow().
```

Returns true if the system supports transparent windows and calling `setTransparent/2`
may succeed.

If this function returns false, transparent windows are definitely not supported by the
current system.

# `captureMouse`

```erlang
-spec captureMouse(This) -> ok when This :: wxWindow().
```

Directs all mouse input to this window.

Call `releaseMouse/1` to release the capture.

Note that wxWidgets maintains the stack of windows having captured the mouse and when the
mouse is released the capture returns to the window which had had captured it previously
and it is only really released if there were no previous window. In particular, this means
that you must release the mouse as many times as you capture it, unless the window
receives the `m:wxMouseCaptureLostEvent` event.

Any application which captures the mouse in the beginning of some operation must handle `m:wxMouseCaptureLostEvent`
and cancel this operation when it receives the event. The event handler must not
recapture mouse.

See:
* `releaseMouse/1`

* `m:wxMouseCaptureLostEvent`

# `center`

```erlang
-spec center(This) -> ok when This :: wxWindow().
```

# `center`

```erlang
-spec center(This, [Option]) -> ok when This :: wxWindow(), Option :: {dir, integer()}.
```

Equivalent to: `centre/2`

# `centerOnParent`

```erlang
-spec centerOnParent(This) -> ok when This :: wxWindow().
```

# `centerOnParent`

```erlang
-spec centerOnParent(This, [Option]) -> ok when This :: wxWindow(), Option :: {dir, integer()}.
```

Equivalent to: `centreOnParent/2`

# `centre`

```erlang
-spec centre(This) -> ok when This :: wxWindow().
```

# `centre`

```erlang
-spec centre(This, [Option]) -> ok when This :: wxWindow(), Option :: {dir, integer()}.
```

Centres the window.

Remark: If the window is a top level one (i.e. doesn't have a parent), it will be centred
relative to the screen anyhow.

See: `center/2`

# `centreOnParent`

```erlang
-spec centreOnParent(This) -> ok when This :: wxWindow().
```

# `centreOnParent`

```erlang
-spec centreOnParent(This, [Option]) -> ok when This :: wxWindow(), Option :: {dir, integer()}.
```

Centres the window on its parent.

This is a more readable synonym for `centre/2`.

Remark: This methods provides for a way to centre top level windows over their parents
instead of the entire screen. If there is no parent or if the window is not a top level
window, then behaviour is the same as `centre/2`.

See: `wxTopLevelWindow:centreOnScreen/2`

# `clearBackground`

```erlang
-spec clearBackground(This) -> ok when This :: wxWindow().
```

Clears the window by filling it with the current background colour.

Does not cause an erase background event to be generated.

Notice that this uses `m:wxClientDC` to draw on the window and the results of doing it
while also drawing on `m:wxPaintDC` for this window are undefined. Hence this method
shouldn't be used from EVT_PAINT handlers, just use `wxDC:clear/1` on the `m:wxPaintDC` you already use
there instead.

# `clientToScreen`

```erlang
-spec clientToScreen(This, Pt) -> {X :: integer(), Y :: integer()}
                        when This :: wxWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

Converts to screen coordinates from coordinates relative to this window.

# `clientToScreen`

```erlang
-spec clientToScreen(This, X, Y) -> {X :: integer(), Y :: integer()}
                        when This :: wxWindow(), X :: integer(), Y :: integer().
```

Converts to screen coordinates from coordinates relative to this window.

# `close`

```erlang
-spec close(This) -> boolean() when This :: wxWindow().
```

# `close`

```erlang
-spec close(This, [Option]) -> boolean() when This :: wxWindow(), Option :: {force, boolean()}.
```

This function simply generates a `m:wxCloseEvent` whose handler usually tries to close
the window.

It doesn't close the window itself, however.

Return: true if the event was handled and not vetoed, false otherwise.

Remark: Close calls the close handler for the window, providing an opportunity for the
window to choose whether to destroy the window. Usually it is only used with the top level
windows (`m:wxFrame` and `m:wxDialog` classes) as the others are not supposed to have any
special OnClose() logic. The close handler should check whether the window is being
deleted forcibly, using `wxCloseEvent:canVeto/1`, in which case it should destroy the window using `'Destroy'/1`. Note that
calling Close does not guarantee that the window will be destroyed; but it provides a way
to simulate a manual close of a window, which may or may not be implemented by destroying
the window. The default implementation of wxDialog::OnCloseWindow does not necessarily
delete the dialog, since it will simply simulate an wxID_CANCEL event which is handled by
the appropriate button event handler and may do anything at all. To guarantee that the
window will be destroyed, call `'Destroy'/1` instead

See:
* `'Destroy'/1`

* `m:wxCloseEvent`

# `convertDialogToPixels`

```erlang
-spec convertDialogToPixels(This, Sz) -> {W :: integer(), H :: integer()}
                               when This :: wxWindow(), Sz :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `convertPixelsToDialog`

```erlang
-spec convertPixelsToDialog(This, Sz) -> {W :: integer(), H :: integer()}
                               when This :: wxWindow(), Sz :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxWindow(), Parent :: wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxWindow(),
                    Parent :: wxWindow(),
                    Id :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Construct the actual window object after creating the C++ object.

The non-default constructor of `m:wxWindow` class does two things: it initializes the C++
object and it also creates the window object in the underlying graphical toolkit. The `create/4`
method can be used to perform the second part later, while the default constructor can be
used to perform the first part only.

Please note that the underlying window must be created exactly once, i.e. if you use the
default constructor, which doesn't do this, you `must` call `create/4` before using the window and
if you use the non-default constructor, you can `not` call `create/4`, as the underlying window is
already created.

Note that it is possible and, in fact, useful, to call some methods on the object between
creating the C++ object itself and calling `create/4` on it, e.g. a common pattern to avoid showing
the contents of a window before it is fully initialized is:

Also note that it is possible to create an object of a derived type and then call `create/4` on it:
This is notably used by overview_xrc.

The parameters of this method have exactly the same meaning as the non-default
constructor parameters, please refer to them for their description.

Return: true if window creation succeeded or false if it failed

# `Destroy`

```erlang
-spec 'Destroy'(This) -> boolean() when This :: wxWindow().
```

Destroys the window safely.

Use this function instead of the delete operator, since different window classes can be
destroyed differently. Frames and dialogs are not destroyed immediately when this function
is called - they are added to a list of windows to be deleted on idle time, when all the
window's events have been processed. This prevents problems with events being sent to
non-existent windows.

Return: true if the window has either been successfully deleted, or it has been added to
the list of windows pending real deletion.

# `destroy`

```erlang
-spec destroy(This :: wxWindow()) -> ok.
```

Destroys the object

# `destroyChildren`

```erlang
-spec destroyChildren(This) -> boolean() when This :: wxWindow().
```

Destroys all children of a window.

Called automatically by the destructor.

# `disable`

```erlang
-spec disable(This) -> boolean() when This :: wxWindow().
```

Disables the window.

Same as `enable/2` Enable(false).

Return: Returns true if the window has been disabled, false if it had been already
disabled before the call to this function.

# `dragAcceptFiles`

```erlang
-spec dragAcceptFiles(This, Accept) -> ok when This :: wxWindow(), Accept :: boolean().
```

Enables or disables eligibility for drop file events (OnDropFiles).

Remark: Windows only until version 2.8.9, available on all platforms since 2.8.10. Cannot
be used together with `setDropTarget/2` on non-Windows platforms.

See: `setDropTarget/2`

# `enable`

```erlang
-spec enable(This) -> boolean() when This :: wxWindow().
```

# `enable`

```erlang
-spec enable(This, [Option]) -> boolean() when This :: wxWindow(), Option :: {enable, boolean()}.
```

Enable or disable the window for user input.

Note that when a parent window is disabled, all of its children are disabled as well and
they are re-enabled again when the parent is.

A window can be created initially disabled by calling this method on it `before` calling `create/4`
to create the actual underlying window, e.g.

Return: Returns true if the window has been enabled or disabled, false if nothing was
done, i.e. if the window had already been in the specified state.

See:
* `isEnabled/1`

* `disable/1`

* `wxRadioBox:enable/3`

# `findFocus`

```erlang
-spec findFocus() -> wxWindow().
```

Finds the window or control which currently has the keyboard focus.

Remark: Note that this is a static function, so it can be called without needing a `m:wxWindow`
pointer.

See: `setFocus/1`

# `findWindow`

```erlang
-spec findWindow(This, Id) -> wxWindow() when This :: wxWindow(), Id :: integer();
                (This, Name) -> wxWindow() when This :: wxWindow(), Name :: unicode:chardata().
```

Find a child of this window, by name.

May return `this` if it matches itself.

Notice that only real children, not top level windows using this window as parent, are
searched by this function.

# `findWindowById`

```erlang
-spec findWindowById(Id) -> wxWindow() when Id :: integer().
```

# `findWindowById`

```erlang
-spec findWindowById(Id, [Option]) -> wxWindow() when Id :: integer(), Option :: {parent, wxWindow()}.
```

Find the first window with the given `id`.

If `parent` is NULL, the search will start from all top-level frames and dialog boxes; if
non-NULL, the search will be limited to the given window hierarchy. The search is
recursive in both cases.

See: `findWindow/2`

Return: Window with the given `id` or NULL if not found.

# `findWindowByLabel`

```erlang
-spec findWindowByLabel(Label) -> wxWindow() when Label :: unicode:chardata().
```

# `findWindowByLabel`

```erlang
-spec findWindowByLabel(Label, [Option]) -> wxWindow()
                           when Label :: unicode:chardata(), Option :: {parent, wxWindow()}.
```

Find a window by its label.

Depending on the type of window, the label may be a window title or panel item label. If `parent`
is NULL, the search will start from all top-level frames and dialog boxes; if non-NULL,
the search will be limited to the given window hierarchy.

The search is recursive in both cases and, unlike with `findWindow/2`, recurses into top level child
windows too.

See: `findWindow/2`

Return: Window with the given `label` or NULL if not found.

# `findWindowByName`

```erlang
-spec findWindowByName(Name) -> wxWindow() when Name :: unicode:chardata().
```

# `findWindowByName`

```erlang
-spec findWindowByName(Name, [Option]) -> wxWindow()
                          when Name :: unicode:chardata(), Option :: {parent, wxWindow()}.
```

Find a window by its name (as given in a window constructor or `create/4` function call).

If `parent` is NULL, the search will start from all top-level frames and dialog boxes; if
non-NULL, the search will be limited to the given window hierarchy.

The search is recursive in both cases and, unlike `findWindow/2`, recurses into top level child windows too.

If no window with such name is found, `findWindowByLabel/2` is called, i.e. the name is interpreted as
(internal) name first but if this fails, it's internal as (user-visible) label. As this
behaviour may be confusing, it is usually better to use either the `findWindow/2` overload taking the
name or `findWindowByLabel/2` directly.

Return: Window with the given `name` or NULL if not found.

# `fit`

```erlang
-spec fit(This) -> ok when This :: wxWindow().
```

Sizes the window to fit its best size.

Using this function is equivalent to setting window size to the return value of `getBestSize/1`.

Note that, unlike `setSizerAndFit/3`, this function only changes the current window size and doesn't change
its minimal size.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `fitInside`

```erlang
-spec fitInside(This) -> ok when This :: wxWindow().
```

Similar to `fit/1`, but sizes the interior (virtual) size of a window.

Mainly useful with scrolled windows to reset scrollbars after sizing changes that do not
trigger a size event, and/or scrolled windows without an interior sizer. This function
similarly won't do anything if there are no subwindows.

# `freeze`

```erlang
-spec freeze(This) -> ok when This :: wxWindow().
```

Freezes the window or, in other words, prevents any updates from taking place on screen,
the window is not redrawn at all.

`thaw/1` must be called to re-enable window redrawing. Calls to these two functions may be nested
but to ensure that the window is properly repainted again, you must thaw it exactly as
many times as you froze it.

If the window has any children, they are recursively frozen too.

This method is useful for visual appearance optimization (for example, it is a good idea
to use it before doing many large text insertions in a row into a `m:wxTextCtrl` under
wxGTK) but is not implemented on all platforms nor for all controls so it is mostly just a
hint to wxWidgets and not a mandatory directive.

See:
* `thaw/1`

* `isFrozen/1`

# `fromDIP`

```erlang
-spec fromDIP(D, W) -> integer() when D :: integer(), W :: wxWindow();
             (Sz, W) -> {W :: integer(), H :: integer()}
                 when Sz :: {W :: integer(), H :: integer()}, W :: wxWindow();
             (This, D) -> integer() when This :: wxWindow(), D :: integer();
             (This, Sz) -> {W :: integer(), H :: integer()}
                 when This :: wxWindow(), Sz :: {W :: integer(), H :: integer()}.
```

Convert DPI-independent pixel values to the value in pixels appropriate for the current
toolkit.

A DPI-independent pixel is just a pixel at the standard 96 DPI resolution. To keep the
same physical size at higher resolution, the physical pixel value must be scaled by `getDPIScaleFactor/1` but
this scaling may be already done by the underlying toolkit (GTK+, Cocoa, ...)
automatically. This method performs the conversion only if it is not already done by the
lower level toolkit and so by using it with pixel values you can guarantee that the
physical size of the corresponding elements will remain the same in all resolutions under
all platforms. For example, instead of creating a bitmap of the hard coded size of 32
pixels you should use to avoid using tiny bitmaps on high DPI screens.

Notice that this function is only needed when using hard coded pixel values. It is not
necessary if the sizes are already based on the DPI-independent units such as dialog units
or if you are relying on the controls automatic best size determination and using sizers
to lay out them.

Also note that if either component of `sz` has the special value of -1, it is returned
unchanged independently of the current DPI, to preserve the special value of -1 in
wxWidgets API (it is often used to mean "unspecified").

Since: 3.1.0

# `getAcceleratorTable`

```erlang
-spec getAcceleratorTable(This) -> wxAcceleratorTable:wxAcceleratorTable() when This :: wxWindow().
```

Gets the accelerator table for this window.

See `m:wxAcceleratorTable`.

# `getBackgroundColour`

```erlang
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxWindow().
```

Returns the background colour of the window.

See:
* `setBackgroundColour/2`

* `setForegroundColour/2`

* `getForegroundColour/1`

# `getBackgroundStyle`

```erlang
-spec getBackgroundStyle(This) -> wx:wx_enum() when This :: wxWindow().
```

Returns the background style of the window.

See:
* `setBackgroundColour/2`

* `getForegroundColour/1`

* `setBackgroundStyle/2`

* `setTransparent/2`

# `getBestSize`

```erlang
-spec getBestSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

This functions returns the best acceptable minimal size for the window.

For example, for a static control, it will be the minimal size such that the control
label is not truncated. For windows containing subwindows (typically `m:wxPanel`), the size
returned by this function will be the same as the size the window would have had after
calling `fit/1`.

Override virtual `DoGetBestSize()` (not implemented in wx) or, better, because it's
usually more convenient, `DoGetBestClientSize()` (not implemented in wx) when writing your
own custom window class to change the value returned by this public non-virtual method.

Notice that the best size respects the minimal and maximal size explicitly set for the
window, if any. So even if some window believes that it needs 200 pixels horizontally,
calling `setMaxSize/2` with a width of 100 would ensure that `getBestSize/1` returns the width of at most 100 pixels.

See:
* `cacheBestSize/2`

* [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `getCapture`

```erlang
-spec getCapture() -> wxWindow().
```

Returns the currently captured window.

See:
* `hasCapture/1`

* `captureMouse/1`

* `releaseMouse/1`

* `m:wxMouseCaptureLostEvent`

* `m:wxMouseCaptureChangedEvent`

# `getCaret`

```erlang
-spec getCaret(This) -> wxCaret:wxCaret() when This :: wxWindow().
```

Returns the caret() associated with the window.

# `getCharHeight`

```erlang
-spec getCharHeight(This) -> integer() when This :: wxWindow().
```

Returns the character height for this window.

# `getCharWidth`

```erlang
-spec getCharWidth(This) -> integer() when This :: wxWindow().
```

Returns the average character width for this window.

# `getChildren`

```erlang
-spec getChildren(This) -> [wxWindow()] when This :: wxWindow().
```

Returns a const reference to the list of the window's children.

`wxWindowList` is a type-safe wxList-like class whose elements are of type `wxWindow*`.

# `getClientSize`

```erlang
-spec getClientSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `getContainingSizer`

```erlang
-spec getContainingSizer(This) -> wxSizer:wxSizer() when This :: wxWindow().
```

Returns the sizer of which this window is a member, if any, otherwise NULL.

# `getContentScaleFactor`

```erlang
-spec getContentScaleFactor(This) -> number() when This :: wxWindow().
```

Returns the factor mapping logical pixels of this window to physical pixels.

This function can be used to portably determine the number of physical pixels in a window
of the given size, by multiplying the window size by the value returned from it. I.e. it
returns the factor converting window coordinates to "content view" coordinates, where the
view can be just a simple window displaying a `m:wxBitmap` or `m:wxGLCanvas` or any other
kind of window rendering arbitrary "content" on screen.

For the platforms not doing any pixel mapping, i.e. where logical and physical pixels are
one and the same, this function always returns 1.0 and so using it is, in principle,
unnecessary and could be avoided by using preprocessor check for `wxHAVE_DPI_INDEPENDENT_PIXELS`
`not` being defined, however using this function unconditionally under all platforms is
usually simpler and so preferable.

Note: Current behaviour of this function is compatible with wxWidgets 3.0, but different
from its behaviour in versions 3.1.0 to 3.1.3, where it returned the same value as `getDPIScaleFactor/1`.
Please use the other function if you need to use a scaling factor greater than 1.0 even
for the platforms without `wxHAVE_DPI_INDEPENDENT_PIXELS`, such as wxMSW.

Since: 2.9.5

# `getCursor`

```erlang
-spec getCursor(This) -> wxCursor:wxCursor() when This :: wxWindow().
```

Return the cursor associated with this window.

See: `setCursor/2`

# `getDPI`

```erlang
-spec getDPI(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

Return the DPI of the display used by this window.

The returned value can be different for different windows on systems with support for
per-monitor DPI values, such as Microsoft Windows 10.

If the DPI is not available, returns `{Width,Height}` object.

See: `wxDisplay:getPPI/1`

Since: 3.1.3

# `getDPIScaleFactor`

```erlang
-spec getDPIScaleFactor(This) -> number() when This :: wxWindow().
```

Returns the ratio of the DPI used by this window to the standard DPI.

The returned value is 1 for standard DPI screens or 2 for "200% scaling" and, unlike for `getContentScaleFactor/1`,
is the same under all platforms.

This factor should be used to increase the size of icons and similar windows whose best
size is not based on text metrics when using DPI scaling.

E.g. the program may load a 32px bitmap if the content scale factor is 1.0 or 64px
version of the same bitmap if it is 2.0 or bigger.

Notice that this method should `not` be used for window sizes expressed in pixels, as
they are already scaled by this factor by the underlying toolkit under some platforms. Use `fromDIP/2`
for anything window-related instead.

Since: 3.1.4

# `getDropTarget`

```erlang
-spec getDropTarget(This) -> wx:wx_object() when This :: wxWindow().
```

Returns the associated drop target, which may be NULL.

See:
* `setDropTarget/2`

* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

# `getExtraStyle`

```erlang
-spec getExtraStyle(This) -> integer() when This :: wxWindow().
```

Returns the extra style bits for the window.

# `getFont`

```erlang
-spec getFont(This) -> wxFont:wxFont() when This :: wxWindow().
```

Returns the font for this window.

See: `setFont/2`

# `getForegroundColour`

```erlang
-spec getForegroundColour(This) -> wx:wx_colour4() when This :: wxWindow().
```

Returns the foreground colour of the window.

Remark: The meaning of foreground colour varies according to the window class; it may be
the text colour or other colour, or it may not be used at all.

See:
* `setForegroundColour/2`

* `setBackgroundColour/2`

* `getBackgroundColour/1`

# `getGrandParent`

```erlang
-spec getGrandParent(This) -> wxWindow() when This :: wxWindow().
```

Returns the grandparent of a window, or NULL if there isn't one.

# `getHandle`

```erlang
-spec getHandle(This) -> integer() when This :: wxWindow().
```

Returns the platform-specific handle of the physical window.

Cast it to an appropriate handle, such as `HWND` for Windows, `Widget` for Motif or `GtkWidget`
for GTK.

# `getHelpText`

```erlang
-spec getHelpText(This) -> unicode:charlist() when This :: wxWindow().
```

Gets the help text to be used as context-sensitive help for this window.

Note that the text is actually stored by the current `wxHelpProvider` (not implemented in
wx) implementation, and not in the window object itself.

See: `setHelpText/2`

# `getId`

```erlang
-spec getId(This) -> integer() when This :: wxWindow().
```

Returns the identifier of the window.

Remark: Each window has an integer identifier. If the application has not provided one
(or the default wxID_ANY) a unique identifier with a negative value will be generated.

See:
* `setId/2`

* [Overview windowids](https://docs.wxwidgets.org/3.2/overview_windowids.html#overview_windowids)

# `getLabel`

```erlang
-spec getLabel(This) -> unicode:charlist() when This :: wxWindow().
```

Generic way of getting a label from any window, for identification purposes.

Remark: The interpretation of this function differs from class to class. For frames and
dialogs, the value returned is the title. For buttons or static text controls, it is the
button text. This function can be useful for meta-programs (such as testing tools or
special-needs access programs) which need to identify windows by name.

# `getMaxSize`

```erlang
-spec getMaxSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

Returns the maximum size of the window.

This is an indication to the sizer layout mechanism that this is the maximum possible
size as well as the upper bound on window's size settable using `setSize/6`.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `getMinSize`

```erlang
-spec getMinSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

Returns the minimum size of the window, an indication to the sizer layout mechanism that
this is the minimum required size.

This method normally just returns the value set by `setMinSize/2`, but it can be overridden to do the
calculation on demand.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `getName`

```erlang
-spec getName(This) -> unicode:charlist() when This :: wxWindow().
```

Returns the window's name.

Remark: This name is not guaranteed to be unique; it is up to the programmer to supply an
appropriate name in the window constructor or via `setName/2`.

See: `setName/2`

# `getParent`

```erlang
-spec getParent(This) -> wxWindow() when This :: wxWindow().
```

Returns the parent of the window, or NULL if there is no parent.

# `getPosition`

```erlang
-spec getPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxWindow().
```

This gets the position of the window in pixels, relative to the parent window for the
child windows or relative to the display origin for the top level windows.

See: `getScreenPosition/1`

# `getRect`

```erlang
-spec getRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                 when This :: wxWindow().
```

Returns the position and size of the window as a {X,Y,W,H} object.

See: `getScreenRect/1`

# `getScreenPosition`

```erlang
-spec getScreenPosition(This) -> {X :: integer(), Y :: integer()} when This :: wxWindow().
```

Returns the window position in screen coordinates, whether the window is a child window
or a top level one.

See: `getPosition/1`

# `getScreenRect`

```erlang
-spec getScreenRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                       when This :: wxWindow().
```

Returns the position and size of the window on the screen as a {X,Y,W,H} object.

See: `getRect/1`

# `getScrollPos`

```erlang
-spec getScrollPos(This, Orientation) -> integer() when This :: wxWindow(), Orientation :: integer().
```

Returns the built-in scrollbar position.

See: `setScrollbar/6`

# `getScrollRange`

```erlang
-spec getScrollRange(This, Orientation) -> integer() when This :: wxWindow(), Orientation :: integer().
```

Returns the built-in scrollbar range.

See: `setScrollbar/6`

# `getScrollThumb`

```erlang
-spec getScrollThumb(This, Orientation) -> integer() when This :: wxWindow(), Orientation :: integer().
```

Returns the built-in scrollbar thumb size.

See: `setScrollbar/6`

# `getSize`

```erlang
-spec getSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

See the GetSize(int*,int*) overload for more info.

# `getSizer`

```erlang
-spec getSizer(This) -> wxSizer:wxSizer() when This :: wxWindow().
```

Returns the sizer associated with the window by a previous call to `setSizer/3`, or NULL.

# `getTextExtent`

```erlang
-spec getTextExtent(This, String) -> Result
                       when
                           Result ::
                               {W :: integer(),
                                H :: integer(),
                                Descent :: integer(),
                                ExternalLeading :: integer()},
                           This :: wxWindow(),
                           String :: unicode:chardata().
```

# `getTextExtent`

```erlang
-spec getTextExtent(This, String, [Option]) -> Result
                       when
                           Result ::
                               {W :: integer(),
                                H :: integer(),
                                Descent :: integer(),
                                ExternalLeading :: integer()},
                           This :: wxWindow(),
                           String :: unicode:chardata(),
                           Option :: {theFont, wxFont:wxFont()}.
```

Gets the dimensions of the string as it would be drawn on the window with the currently
selected font.

The text extent is returned in the `w` and `h` pointers.

# `getThemeEnabled`

```erlang
-spec getThemeEnabled(This) -> boolean() when This :: wxWindow().
```

Returns true if the window uses the system theme for drawing its background.

See: `setThemeEnabled/2`

# `getToolTip`

```erlang
-spec getToolTip(This) -> wxToolTip:wxToolTip() when This :: wxWindow().
```

Get the associated tooltip or NULL if none.

# `getUpdateRegion`

```erlang
-spec getUpdateRegion(This) -> wxRegion:wxRegion() when This :: wxWindow().
```

Gets the dimensions of the string as it would be drawn on the window with the currently
selected font.

Returns the region specifying which parts of the window have been damaged. Should only be
called within an `m:wxPaintEvent` handler.

See: `m:wxRegion`

# `getVirtualSize`

```erlang
-spec getVirtualSize(This) -> {W :: integer(), H :: integer()} when This :: wxWindow().
```

This gets the virtual size of the window in pixels.

By default it returns the client size of the window, but after a call to `setVirtualSize/3` it will return
the size set with that method.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `getWindowStyleFlag`

```erlang
-spec getWindowStyleFlag(This) -> integer() when This :: wxWindow().
```

Gets the window style that was passed to the constructor or `create/4` method.

`GetWindowStyle()` (not implemented in wx) is another name for the same function.

# `getWindowVariant`

```erlang
-spec getWindowVariant(This) -> wx:wx_enum() when This :: wxWindow().
```

Returns the value previously passed to `setWindowVariant/2`.

# `hasCapture`

```erlang
-spec hasCapture(This) -> boolean() when This :: wxWindow().
```

Returns true if this window has the current mouse capture.

See:
* `captureMouse/1`

* `releaseMouse/1`

* `m:wxMouseCaptureLostEvent`

* `m:wxMouseCaptureChangedEvent`

# `hasScrollbar`

```erlang
-spec hasScrollbar(This, Orient) -> boolean() when This :: wxWindow(), Orient :: integer().
```

Returns true if this window currently has a scroll bar for this orientation.

This method may return false even when `CanScroll()` (not implemented in wx) for the same
orientation returns true, but if `CanScroll()` (not implemented in wx) returns false, i.e.
scrolling in this direction is not enabled at all, `hasScrollbar/2` always returns false as well.

# `hasTransparentBackground`

```erlang
-spec hasTransparentBackground(This) -> boolean() when This :: wxWindow().
```

Returns true if this window background is transparent (as, for example, for `m:wxStaticText`)
and should show the parent window background.

This method is mostly used internally by the library itself and you normally shouldn't
have to call it. You may, however, have to override it in your wxWindow-derived class to
ensure that background is painted correctly.

# `hide`

```erlang
-spec hide(This) -> boolean() when This :: wxWindow().
```

Equivalent to calling `show/2`(false).

# `inheritAttributes`

```erlang
-spec inheritAttributes(This) -> ok when This :: wxWindow().
```

This function is (or should be, in case of custom controls) called during window creation
to intelligently set up the window visual attributes, that is the font and the foreground
and background colours.

By "intelligently" the following is meant: by default, all windows use their own `GetClassDefaultAttributes()`
(not implemented in wx) default attributes. However if some of the parents attributes are
explicitly (that is, using `setFont/2` and not `setOwnFont/2`) changed and if the corresponding attribute hadn't
been explicitly set for this window itself, then this window takes the same value as used
by the parent. In addition, if the window overrides `shouldInheritColours/1` to return false, the colours will not
be changed no matter what and only the font might.

This rather complicated logic is necessary in order to accommodate the different usage
scenarios. The most common one is when all default attributes are used and in this case,
nothing should be inherited as in modern GUIs different controls use different fonts (and
colours) than their siblings so they can't inherit the same value from the parent. However
it was also deemed desirable to allow to simply change the attributes of all children at
once by just changing the font or colour of their common parent, hence in this case we do
inherit the parents attributes.

# `initDialog`

```erlang
-spec initDialog(This) -> ok when This :: wxWindow().
```

Sends an `wxEVT\_INIT\_DIALOG` event, whose handler usually transfers data to the dialog
via validators.

# `invalidateBestSize`

```erlang
-spec invalidateBestSize(This) -> ok when This :: wxWindow().
```

Resets the cached best size value so it will be recalculated the next time it is needed.

See: `cacheBestSize/2`

# `isDoubleBuffered`

```erlang
-spec isDoubleBuffered(This) -> boolean() when This :: wxWindow().
```

Returns true if the window contents is double-buffered by the system, i.e. if any drawing
done on the window is really done on a temporary backing surface and transferred to the
screen all at once later.

See: `m:wxBufferedDC`

# `isEnabled`

```erlang
-spec isEnabled(This) -> boolean() when This :: wxWindow().
```

Returns true if the window is enabled, i.e. if it accepts user input, false otherwise.

Notice that this method can return false even if this window itself hadn't been
explicitly disabled when one of its parent windows is disabled. To get the intrinsic
status of this window, use `IsThisEnabled()` (not implemented in wx)

See: `enable/2`

# `isExposed`

```erlang
-spec isExposed(This, Pt) -> boolean() when This :: wxWindow(), Pt :: {X :: integer(), Y :: integer()};
               (This, Rect) -> boolean()
                   when
                       This :: wxWindow(),
                       Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `isExposed`

```erlang
-spec isExposed(This, X, Y) -> boolean() when This :: wxWindow(), X :: integer(), Y :: integer().
```

Returns true if the given point or rectangle area has been exposed since the last
repaint.

Call this in an paint event handler to optimize redrawing by only redrawing those areas,
which have been exposed.

# `isExposed`

```erlang
-spec isExposed(This, X, Y, W, H) -> boolean()
                   when
                       This :: wxWindow(),
                       X :: integer(),
                       Y :: integer(),
                       W :: integer(),
                       H :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `isFrozen`

```erlang
-spec isFrozen(This) -> boolean() when This :: wxWindow().
```

Returns true if the window is currently frozen by a call to `freeze/1`.

See:
* `freeze/1`

* `thaw/1`

# `isRetained`

```erlang
-spec isRetained(This) -> boolean() when This :: wxWindow().
```

Returns true if the window is retained, false otherwise.

Remark: Retained windows are only available on X platforms.

# `isShown`

```erlang
-spec isShown(This) -> boolean() when This :: wxWindow().
```

Returns true if the window is shown, false if it has been hidden.

See: `isShownOnScreen/1`

# `isShownOnScreen`

```erlang
-spec isShownOnScreen(This) -> boolean() when This :: wxWindow().
```

Returns true if the window is physically visible on the screen, i.e. it is shown and all
its parents up to the toplevel window are shown as well.

See: `isShown/1`

# `isTopLevel`

```erlang
-spec isTopLevel(This) -> boolean() when This :: wxWindow().
```

Returns true if the given window is a top-level one.

Currently all frames and dialogs are considered to be top-level windows (even if they
have a parent window).

# `layout`

```erlang
-spec layout(This) -> boolean() when This :: wxWindow().
```

Lays out the children of this window using the associated sizer.

If a sizer hadn't been associated with this window (see `setSizer/3`), this function doesn't do
anything, unless this is a top level window (see `layout/1`).

Note that this method is called automatically when the window size changes if it has the
associated sizer (or if `setAutoLayout/2` with true argument had been explicitly called), ensuring that it
is always laid out correctly.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

Return: Always returns true, the return value is not useful.

# `lineDown`

```erlang
-spec lineDown(This) -> boolean() when This :: wxWindow().
```

Same as `scrollLines/2` (1).

# `lineUp`

```erlang
-spec lineUp(This) -> boolean() when This :: wxWindow().
```

Same as `scrollLines/2` (-1).

# `lower`

```erlang
-spec lower(This) -> ok when This :: wxWindow().
```

Lowers the window to the bottom of the window hierarchy (Z-order).

Remark: This function only works for wxTopLevelWindow-derived classes.

See: `raise/1`

# `move`

```erlang
-spec move(This, Pt) -> ok when This :: wxWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

# `move`

```erlang
-spec move(This, X, Y) -> ok when This :: wxWindow(), X :: integer(), Y :: integer();
          (This, Pt, [Option]) -> ok
              when
                  This :: wxWindow(),
                  Pt :: {X :: integer(), Y :: integer()},
                  Option :: {flags, integer()}.
```

Moves the window to the given position.

Remark: Implementations of `setSize/6` can also implicitly implement the `move/4` function, which is defined
in the base `m:wxWindow` class as the call:

See: `setSize/6`

# `move`

```erlang
-spec move(This, X, Y, [Option]) -> ok
              when This :: wxWindow(), X :: integer(), Y :: integer(), Option :: {flags, integer()}.
```

Moves the window to the given position.

Remark: Implementations of SetSize can also implicitly implement the `move/4` function, which is
defined in the base `m:wxWindow` class as the call:

See: `setSize/6`

# `moveAfterInTabOrder`

```erlang
-spec moveAfterInTabOrder(This, Win) -> ok when This :: wxWindow(), Win :: wxWindow().
```

Moves this window in the tab navigation order after the specified `win`.

This means that when the user presses `TAB` key on that other window, the focus switches
to this window.

Default tab order is the same as creation order, this function and `moveBeforeInTabOrder/2` allow to change it
after creating all the windows.

# `moveBeforeInTabOrder`

```erlang
-spec moveBeforeInTabOrder(This, Win) -> ok when This :: wxWindow(), Win :: wxWindow().
```

Same as `moveAfterInTabOrder/2` except that it inserts this window just before `win`
instead of putting it right after it.

# `navigate`

```erlang
-spec navigate(This) -> boolean() when This :: wxWindow().
```

# `navigate`

```erlang
-spec navigate(This, [Option]) -> boolean() when This :: wxWindow(), Option :: {flags, integer()}.
```

Performs a keyboard navigation action starting from this window.

This method is equivalent to calling `NavigateIn()` (not implemented in wx) method on the
parent window.

Return: Returns true if the focus was moved to another window or false if nothing changed.

Remark: You may wish to call this from a text control custom keypress handler to do the
default navigation behaviour for the tab key, since the standard default behaviour for a
multiline text control with the wxTE_PROCESS_TAB style is to insert a tab and not navigate
to the next control. See also `m:wxNavigationKeyEvent` and HandleAsNavigationKey.

# `new`

```erlang
-spec new() -> wxWindow().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxWindow() when Parent :: wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxWindow()
             when
                 Parent :: wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructs a window, which can be a child of a frame, dialog or any other non-control
window.

# `pageDown`

```erlang
-spec pageDown(This) -> boolean() when This :: wxWindow().
```

Same as `scrollPages/2` (1).

# `pageUp`

```erlang
-spec pageUp(This) -> boolean() when This :: wxWindow().
```

Same as `scrollPages/2` (-1).

# `popupMenu`

```erlang
-spec popupMenu(This, Menu) -> boolean() when This :: wxWindow(), Menu :: wxMenu:wxMenu().
```

# `popupMenu`

```erlang
-spec popupMenu(This, Menu, [Option]) -> boolean()
                   when
                       This :: wxWindow(),
                       Menu :: wxMenu:wxMenu(),
                       Option :: {pos, {X :: integer(), Y :: integer()}}.
```

Pops up the given menu at the specified coordinates, relative to this window, and returns
control when the user has dismissed the menu.

If a menu item is selected, the corresponding menu event is generated and will be
processed as usual. If coordinates are not specified, the current mouse cursor position is used.

`menu` is the menu to pop up.

The position where the menu will appear can be specified either as a {X,Y} `pos` or by
two integers (`x` and `y`).

Note that this function switches focus to this window before showing the menu.

Remark: Just before the menu is popped up, `wxMenu::UpdateUI` (not implemented in wx) is
called to ensure that the menu items are in the correct state. The menu does not get
deleted by the window. It is recommended to not explicitly specify coordinates when
calling PopupMenu in response to mouse click, because some of the ports (namely, wxGTK)
can do a better job of positioning the menu in that case.

See: `m:wxMenu`

# `popupMenu`

```erlang
-spec popupMenu(This, Menu, X, Y) -> boolean()
                   when This :: wxWindow(), Menu :: wxMenu:wxMenu(), X :: integer(), Y :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `raise`

```erlang
-spec raise(This) -> ok when This :: wxWindow().
```

Raises the window to the top of the window hierarchy (Z-order).

Notice that this function only requests the window manager to raise this window to the
top of Z-order. Depending on its configuration, the window manager may raise the window,
not do it at all or indicate that a window requested to be raised in some other way, e.g.
by flashing its icon if it is minimized.

Remark: This function only works for wxTopLevelWindow-derived classes.

See: `lower/1`

# `refresh`

```erlang
-spec refresh(This) -> ok when This :: wxWindow().
```

# `refresh`

```erlang
-spec refresh(This, [Option]) -> ok
                 when
                     This :: wxWindow(),
                     Option ::
                         {eraseBackground, boolean()} |
                         {rect, {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}}.
```

Causes this window, and all of its children recursively (except under wxGTK1 where this
is not implemented), to be repainted.

Note that repainting doesn't happen immediately but only during the next event loop
iteration, if you need to update the window immediately you should use `update/1` instead.

See: `refreshRect/3`

# `refreshRect`

```erlang
-spec refreshRect(This, Rect) -> ok
                     when
                         This :: wxWindow(),
                         Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

# `refreshRect`

```erlang
-spec refreshRect(This, Rect, [Option]) -> ok
                     when
                         This :: wxWindow(),
                         Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                         Option :: {eraseBackground, boolean()}.
```

Redraws the contents of the given rectangle: only the area inside it will be repainted.

This is the same as `refresh/2` but has a nicer syntax as it can be called with a temporary
{X,Y,W,H} object as argument like this `RefreshRect(wxRect(x, y, w, h))`.

# `releaseMouse`

```erlang
-spec releaseMouse(This) -> ok when This :: wxWindow().
```

Releases mouse input captured with `captureMouse/1`.

See:
* `captureMouse/1`

* `hasCapture/1`

* `releaseMouse/1`

* `m:wxMouseCaptureLostEvent`

* `m:wxMouseCaptureChangedEvent`

# `removeChild`

```erlang
-spec removeChild(This, Child) -> ok when This :: wxWindow(), Child :: wxWindow().
```

Removes a child window.

This is called automatically by window deletion functions so should not be required by
the application programmer. Notice that this function is mostly internal to wxWidgets and
shouldn't be called by the user code.

# `reparent`

```erlang
-spec reparent(This, NewParent) -> boolean() when This :: wxWindow(), NewParent :: wxWindow().
```

Reparents the window, i.e. the window will be removed from its current parent window
(e.g.

a non-standard toolbar in a `m:wxFrame`) and then re-inserted into another.

Notice that currently you need to explicitly call `wxBookCtrlBase:removePage/2` before reparenting a notebook page.

# `screenToClient`

```erlang
-spec screenToClient(This) -> {X :: integer(), Y :: integer()} when This :: wxWindow().
```

Converts from screen to client window coordinates.

# `screenToClient`

```erlang
-spec screenToClient(This, Pt) -> {X :: integer(), Y :: integer()}
                        when This :: wxWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

Converts from screen to client window coordinates.

# `scrollLines`

```erlang
-spec scrollLines(This, Lines) -> boolean() when This :: wxWindow(), Lines :: integer().
```

Scrolls the window by the given number of lines down (if `lines` is positive) or up.

Return: Returns true if the window was scrolled, false if it was already on top/bottom
and nothing was done.

Remark: This function is currently only implemented under MSW and `m:wxTextCtrl` under
wxGTK (it also works for `wxScrolled` (not implemented in wx) classes under all platforms).

See: `scrollPages/2`

# `scrollPages`

```erlang
-spec scrollPages(This, Pages) -> boolean() when This :: wxWindow(), Pages :: integer().
```

Scrolls the window by the given number of pages down (if `pages` is positive) or up.

Return: Returns true if the window was scrolled, false if it was already on top/bottom
and nothing was done.

Remark: This function is currently only implemented under MSW and wxGTK.

See: `scrollLines/2`

# `scrollWindow`

```erlang
-spec scrollWindow(This, Dx, Dy) -> ok when This :: wxWindow(), Dx :: integer(), Dy :: integer().
```

# `scrollWindow`

```erlang
-spec scrollWindow(This, Dx, Dy, [Option]) -> ok
                      when
                          This :: wxWindow(),
                          Dx :: integer(),
                          Dy :: integer(),
                          Option ::
                              {rect, {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}}.
```

Physically scrolls the pixels in the window and move child windows accordingly.

Remark: Note that you can often use `wxScrolled` (not implemented in wx) instead of using
this function directly.

# `setAcceleratorTable`

```erlang
-spec setAcceleratorTable(This, Accel) -> ok
                             when This :: wxWindow(), Accel :: wxAcceleratorTable:wxAcceleratorTable().
```

Sets the accelerator table for this window.

See `m:wxAcceleratorTable`.

# `setAutoLayout`

```erlang
-spec setAutoLayout(This, AutoLayout) -> ok when This :: wxWindow(), AutoLayout :: boolean().
```

Determines whether the `layout/1` function will be called automatically when the window
is resized.

This method is called implicitly by `setSizer/3` but if you use `SetConstraints()` (not implemented
in wx) you should call it manually or otherwise the window layout won't be correctly
updated when its size changes.

See: `setSizer/3`

# `setBackgroundColour`

```erlang
-spec setBackgroundColour(This, Colour) -> boolean() when This :: wxWindow(), Colour :: wx:wx_colour().
```

Sets the background colour of the window.

Notice that as with `setForegroundColour/2`, setting the background colour of a native control may not affect
the entire control and could be not supported at all depending on the control and platform.

Please see `inheritAttributes/1` for explanation of the difference between this method and `setOwnBackgroundColour/2`.

Remark: The background colour is usually painted by the default `m:wxEraseEvent` event
handler function under Windows and automatically under GTK. Note that setting the
background colour does not cause an immediate refresh, so you may wish to call `clearBackground/1` or `refresh/2` after
calling this function. Using this function will disable attempts to use themes for this
window, if the system supports them. Use with care since usually the themes represent the
appearance chosen by the user to be used for all applications on the system.

Return: true if the colour was really changed, false if it was already set to this colour
and nothing was done.

See:
* `getBackgroundColour/1`

* `setForegroundColour/2`

* `getForegroundColour/1`

* `clearBackground/1`

* `refresh/2`

* `m:wxEraseEvent`

* `m:wxSystemSettings`

# `setBackgroundStyle`

```erlang
-spec setBackgroundStyle(This, Style) -> boolean() when This :: wxWindow(), Style :: wx:wx_enum().
```

Sets the background style of the window.

The default background style is `wxBG_STYLE_ERASE` which indicates that the window
background may be erased in `EVT_ERASE_BACKGROUND` handler. This is a safe, compatibility
default; however you may want to change it to `wxBG_STYLE_SYSTEM` if you don't define any
erase background event handlers at all, to avoid unnecessary generation of erase
background events and always let system erase the background. And you should change the
background style to `wxBG_STYLE_PAINT` if you define an `EVT_PAINT` handler which
completely overwrites the window background as in this case erasing it previously, either
in `EVT_ERASE_BACKGROUND` handler or in the system default handler, would result in
flicker as the background pixels will be repainted twice every time the window is redrawn.
Do ensure that the background is entirely erased by your `EVT_PAINT` handler in this case
however as otherwise garbage may be left on screen.

Notice that in previous versions of wxWidgets a common way to work around the above
mentioned flickering problem was to define an empty `EVT_ERASE_BACKGROUND` handler.
Setting background style to `wxBG_STYLE_PAINT` is a simpler and more efficient solution to
the same problem.

Under wxGTK and wxOSX, you can use ?wxBG\_STYLE\_TRANSPARENT to obtain full transparency
of the window background. Note that wxGTK supports this only since GTK 2.12 with a
compositing manager enabled, call `IsTransparentBackgroundSupported()` (not implemented in
wx) to check whether this is the case.

Also, in order for `SetBackgroundStyle(wxBG_STYLE_TRANSPARENT)` to work, it must be
called before `create/4`. If you're using your own wxWindow-derived class you should write your code
in the following way:

See:
* `setBackgroundColour/2`

* `getForegroundColour/1`

* `setTransparent/2`

# `setCaret`

```erlang
-spec setCaret(This, Caret) -> ok when This :: wxWindow(), Caret :: wxCaret:wxCaret().
```

Sets the caret() associated with the window.

# `setClientSize`

```erlang
-spec setClientSize(This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()};
                   (This, Rect) -> ok
                       when
                           This :: wxWindow(),
                           Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setClientSize`

```erlang
-spec setClientSize(This, Width, Height) -> ok
                       when This :: wxWindow(), Width :: integer(), Height :: integer().
```

This sets the size of the window client area in pixels.

Using this function to size a window tends to be more device-independent than `setSize/6`, since the
application need not worry about what dimensions the border or title bar have when trying
to fit the window around panel items, for example.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setContainingSizer`

```erlang
-spec setContainingSizer(This, Sizer) -> ok when This :: wxWindow(), Sizer :: wxSizer:wxSizer().
```

Used by `m:wxSizer` internally to notify the window about being managed by the given
sizer.

This method should not be called from outside the library, unless you're implementing a
custom sizer class - and in the latter case you must call this method with the pointer to
the sizer itself whenever a window is added to it and with NULL argument when the window
is removed from it.

# `setCursor`

```erlang
-spec setCursor(This, Cursor) -> boolean() when This :: wxWindow(), Cursor :: wxCursor:wxCursor().
```

Sets the window's cursor.

Notice that the window cursor also sets it for the children of the window implicitly.

The `cursor` may be `wxNullCursor` in which case the window cursor will be reset back to default.

See:
* `wx_misc:setCursor/1`

* `m:wxCursor`

# `setDoubleBuffered`

```erlang
-spec setDoubleBuffered(This, On) -> ok when This :: wxWindow(), On :: boolean().
```

Turn on or off double buffering of the window if the system supports it.

# `setDropTarget`

```erlang
-spec setDropTarget(This, Target) -> ok when This :: wxWindow(), Target :: wx:wx_object().
```

Associates a drop target with this window.

If the window already has a drop target, it is deleted.

See:
* `getDropTarget/1`

* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

# `setExtraStyle`

```erlang
-spec setExtraStyle(This, ExStyle) -> ok when This :: wxWindow(), ExStyle :: integer().
```

Sets the extra style bits for the window.

The currently defined extra style bits are reported in the class description.

# `setFocus`

```erlang
-spec setFocus(This) -> ok when This :: wxWindow().
```

This sets the window to receive keyboard input.

See:
* `m:wxFocusEvent`

* `setFocus/1`

* `wxPanel:setFocusIgnoringChildren/1`

# `setFocusFromKbd`

```erlang
-spec setFocusFromKbd(This) -> ok when This :: wxWindow().
```

This function is called by wxWidgets keyboard navigation code when the user gives the
focus to this window from keyboard (e.g. using `TAB` key).

By default this method simply calls `setFocus/1` but can be overridden to do something in addition to
this in the derived classes.

# `setFont`

```erlang
-spec setFont(This, Font) -> boolean() when This :: wxWindow(), Font :: wxFont:wxFont().
```

Sets the font for this window.

This function should not be called for the parent window if you don't want its font to be
inherited by its children, use `setOwnFont/2` instead in this case and see `inheritAttributes/1` for more explanations.

Please notice that the given font is not automatically used for `m:wxPaintDC` objects
associated with this window, you need to call `wxDC:setFont/2` too. However this font is used by any
standard controls for drawing their text as well as by `getTextExtent/3`.

Return: true if the font was really changed, false if it was already set to this font and
nothing was done.

See:
* `getFont/1`

* `inheritAttributes/1`

# `setForegroundColour`

```erlang
-spec setForegroundColour(This, Colour) -> boolean() when This :: wxWindow(), Colour :: wx:wx_colour().
```

Sets the foreground colour of the window.

The meaning of foreground colour varies according to the window class; it may be the text
colour or other colour, or it may not be used at all. Additionally, not all native
controls support changing their foreground colour so this method may change their colour
only partially or even not at all.

Please see `inheritAttributes/1` for explanation of the difference between this method and `setOwnForegroundColour/2`.

Return: true if the colour was really changed, false if it was already set to this colour
and nothing was done.

See:
* `getForegroundColour/1`

* `setBackgroundColour/2`

* `getBackgroundColour/1`

* `shouldInheritColours/1`

# `setHelpText`

```erlang
-spec setHelpText(This, HelpText) -> ok when This :: wxWindow(), HelpText :: unicode:chardata().
```

Sets the help text to be used as context-sensitive help for this window.

Note that the text is actually stored by the current `wxHelpProvider` (not implemented in
wx) implementation, and not in the window object itself.

See: `getHelpText/1`

# `setId`

```erlang
-spec setId(This, Winid) -> ok when This :: wxWindow(), Winid :: integer().
```

Sets the identifier of the window.

Remark: Each window has an integer identifier. If the application has not provided one,
an identifier will be generated. Normally, the identifier should be provided on creation
and should not be modified subsequently.

See:
* `getId/1`

* [Overview windowids](https://docs.wxwidgets.org/3.2/overview_windowids.html#overview_windowids)

# `setLabel`

```erlang
-spec setLabel(This, Label) -> ok when This :: wxWindow(), Label :: unicode:chardata().
```

Sets the window's label.

See: `getLabel/1`

# `setMaxSize`

```erlang
-spec setMaxSize(This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

Sets the maximum size of the window, to indicate to the sizer layout mechanism that this
is the maximum possible size.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setMinSize`

```erlang
-spec setMinSize(This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

Sets the minimum size of the window, to indicate to the sizer layout mechanism that this
is the minimum required size.

You may need to call this if you change the window size after construction and before
adding to its parent sizer.

Notice that calling this method doesn't prevent the program from making the window
explicitly smaller than the specified size by calling `setSize/6`, it just ensures that it won't
become smaller than this size during the automatic layout.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setName`

```erlang
-spec setName(This, Name) -> ok when This :: wxWindow(), Name :: unicode:chardata().
```

Sets the window's name.

See: `getName/1`

# `setOwnBackgroundColour`

```erlang
-spec setOwnBackgroundColour(This, Colour) -> ok when This :: wxWindow(), Colour :: wx:wx_colour().
```

Sets the background colour of the window but prevents it from being inherited by the
children of this window.

See:
* `setBackgroundColour/2`

* `inheritAttributes/1`

# `setOwnFont`

```erlang
-spec setOwnFont(This, Font) -> ok when This :: wxWindow(), Font :: wxFont:wxFont().
```

Sets the font of the window but prevents it from being inherited by the children of this
window.

See:
* `setFont/2`

* `inheritAttributes/1`

# `setOwnForegroundColour`

```erlang
-spec setOwnForegroundColour(This, Colour) -> ok when This :: wxWindow(), Colour :: wx:wx_colour().
```

Sets the foreground colour of the window but prevents it from being inherited by the
children of this window.

See:
* `setForegroundColour/2`

* `inheritAttributes/1`

# `setPalette`

```erlang
-spec setPalette(This, Pal) -> ok when This :: wxWindow(), Pal :: wxPalette:wxPalette().
```

Deprecated:

use `wxDC:setPalette/2` instead.

# `setScrollbar`

```erlang
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range) -> ok
                      when
                          This :: wxWindow(),
                          Orientation :: integer(),
                          Position :: integer(),
                          ThumbSize :: integer(),
                          Range :: integer().
```

# `setScrollbar`

```erlang
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range, [Option]) -> ok
                      when
                          This :: wxWindow(),
                          Orientation :: integer(),
                          Position :: integer(),
                          ThumbSize :: integer(),
                          Range :: integer(),
                          Option :: {refresh, boolean()}.
```

Sets the scrollbar properties of a built-in scrollbar.

Remark: Let's say you wish to display 50 lines of text, using the same font. The window
is sized so that you can only see 16 lines at a time. You would use: Note that with the
window at this size, the thumb position can never go above 50 minus 16, or 34. You can
determine how many lines are currently visible by dividing the current view size by the
character height in pixels. When defining your own scrollbar behaviour, you will always
need to recalculate the scrollbar settings when the window size changes. You could
therefore put your scrollbar calculations and SetScrollbar call into a function named
AdjustScrollbars, which can be called initially and also from your `m:wxSizeEvent` handler function.

See:
* [Overview scrolling](https://docs.wxwidgets.org/3.2/overview_scrolling.html#overview_scrolling)

* `m:wxScrollBar`

* `m:wxScrollWinEvent`

# `setScrollPos`

```erlang
-spec setScrollPos(This, Orientation, Pos) -> ok
                      when This :: wxWindow(), Orientation :: integer(), Pos :: integer().
```

# `setScrollPos`

```erlang
-spec setScrollPos(This, Orientation, Pos, [Option]) -> ok
                      when
                          This :: wxWindow(),
                          Orientation :: integer(),
                          Pos :: integer(),
                          Option :: {refresh, boolean()}.
```

Sets the position of one of the built-in scrollbars.

Remark: This function does not directly affect the contents of the window: it is up to
the application to take note of scrollbar attributes and redraw contents accordingly.

See:
* `setScrollbar/6`

* `getScrollPos/2`

* `getScrollThumb/2`

* `m:wxScrollBar`

# `setSize`

```erlang
-spec setSize(This, Rect) -> ok
                 when
                     This :: wxWindow(),
                     Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()};
             (This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setSize`

```erlang
-spec setSize(This, Width, Height) -> ok
                 when This :: wxWindow(), Width :: integer(), Height :: integer();
             (This, Rect, [Option]) -> ok
                 when
                     This :: wxWindow(),
                     Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()},
                     Option :: {sizeFlags, integer()}.
```

Sets the size of the window in pixels.

The size is specified using a {X,Y,W,H}, {Width,Height} or by a couple of `int` objects.

Remark: This form must be used with non-default width and height values.

See:
* `move/4`

* [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setSize`

```erlang
-spec setSize(This, X, Y, Width, Height) -> ok
                 when
                     This :: wxWindow(),
                     X :: integer(),
                     Y :: integer(),
                     Width :: integer(),
                     Height :: integer().
```

# `setSize`

```erlang
-spec setSize(This, X, Y, Width, Height, [Option]) -> ok
                 when
                     This :: wxWindow(),
                     X :: integer(),
                     Y :: integer(),
                     Width :: integer(),
                     Height :: integer(),
                     Option :: {sizeFlags, integer()}.
```

Sets the size of the window in pixels.

Remark: This overload sets the position and optionally size, of the window. Parameters
may be wxDefaultCoord to indicate either that a default should be supplied by wxWidgets,
or that the current value of the dimension should be used.

See:
* `move/4`

* [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setSizeHints`

```erlang
-spec setSizeHints(This, MinSize) -> ok
                      when This :: wxWindow(), MinSize :: {W :: integer(), H :: integer()}.
```

# `setSizeHints`

```erlang
-spec setSizeHints(This, MinW, MinH) -> ok when This :: wxWindow(), MinW :: integer(), MinH :: integer();
                  (This, MinSize, [Option]) -> ok
                      when
                          This :: wxWindow(),
                          MinSize :: {W :: integer(), H :: integer()},
                          Option ::
                              {maxSize, {W :: integer(), H :: integer()}} |
                              {incSize, {W :: integer(), H :: integer()}}.
```

Use of this function for windows which are not toplevel windows (such as `m:wxDialog` or `m:wxFrame`)
is discouraged.

Please use `setMinSize/2` and `setMaxSize/2` instead.

See:
* `setSizeHints/4`

* [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setSizeHints`

```erlang
-spec setSizeHints(This, MinW, MinH, [Option]) -> ok
                      when
                          This :: wxWindow(),
                          MinW :: integer(),
                          MinH :: integer(),
                          Option ::
                              {maxW, integer()} |
                              {maxH, integer()} |
                              {incW, integer()} |
                              {incH, integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setSizer`

```erlang
-spec setSizer(This, Sizer) -> ok when This :: wxWindow(), Sizer :: wxSizer:wxSizer().
```

# `setSizer`

```erlang
-spec setSizer(This, Sizer, [Option]) -> ok
                  when This :: wxWindow(), Sizer :: wxSizer:wxSizer(), Option :: {deleteOld, boolean()}.
```

Sets the window to have the given layout sizer.

The window will then own the object, and will take care of its deletion. If an existing
layout constraints object is already owned by the window, it will be deleted if the `deleteOld`
parameter is true.

Note that this function will also call `setAutoLayout/2` implicitly with true parameter if the `sizer` is
non-NULL and false otherwise so that the sizer will be effectively used to layout the
window children whenever it is resized.

Remark: SetSizer enables and disables Layout automatically.

# `setSizerAndFit`

```erlang
-spec setSizerAndFit(This, Sizer) -> ok when This :: wxWindow(), Sizer :: wxSizer:wxSizer().
```

# `setSizerAndFit`

```erlang
-spec setSizerAndFit(This, Sizer, [Option]) -> ok
                        when
                            This :: wxWindow(),
                            Sizer :: wxSizer:wxSizer(),
                            Option :: {deleteOld, boolean()}.
```

Associate the sizer with the window and set the window size and minimal size accordingly.

This method calls `setSizer/3` and then `wxSizer:setSizeHints/2` which sets the initial window size to the size needed to
accommodate all sizer elements and sets the minimal size to the same size, this preventing
the user from resizing this window to be less than this minimal size (if it's a top-level
window which can be directly resized by the user).

# `setThemeEnabled`

```erlang
-spec setThemeEnabled(This, Enable) -> ok when This :: wxWindow(), Enable :: boolean().
```

This function tells a window if it should use the system's "theme" code to draw the
windows' background instead of its own background drawing code.

This does not always have any effect since the underlying platform obviously needs to
support the notion of themes in user defined windows. One such platform is GTK+ where
windows can have (very colourful) backgrounds defined by a user's selected theme.

Dialogs, notebook pages and the status bar have this flag set to true by default so that
the default look and feel is simulated best.

See: `getThemeEnabled/1`

# `setToolTip`

```erlang
-spec setToolTip(This, TipString) -> ok when This :: wxWindow(), TipString :: unicode:chardata();
                (This, Tip) -> ok when This :: wxWindow(), Tip :: wxToolTip:wxToolTip().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setTransparent`

```erlang
-spec setTransparent(This, Alpha) -> boolean() when This :: wxWindow(), Alpha :: integer().
```

Set the transparency of the window.

If the system supports transparent windows, returns true, otherwise returns false and the
window remains fully opaque. See also `canSetTransparent/1`.

The parameter `alpha` is in the range 0..255 where 0 corresponds to a fully transparent
window and 255 to the fully opaque one. The constants `wxIMAGE_ALPHA_TRANSPARENT` and `wxIMAGE_ALPHA_OPAQUE`
can be used.

# `setVirtualSize`

```erlang
-spec setVirtualSize(This, Size) -> ok when This :: wxWindow(), Size :: {W :: integer(), H :: integer()}.
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `setVirtualSize`

```erlang
-spec setVirtualSize(This, Width, Height) -> ok
                        when This :: wxWindow(), Width :: integer(), Height :: integer().
```

Sets the virtual size of the window in pixels.

See: [Overview windowsizing](https://docs.wxwidgets.org/3.2/overview_windowsizing.html#overview_windowsizing)

# `setWindowStyle`

```erlang
-spec setWindowStyle(This, Style) -> ok when This :: wxWindow(), Style :: integer().
```

See `setWindowStyleFlag/2` for more info.

# `setWindowStyleFlag`

```erlang
-spec setWindowStyleFlag(This, Style) -> ok when This :: wxWindow(), Style :: integer().
```

Sets the style of the window.

Please note that some styles cannot be changed after the window creation and that `refresh/2` might
need to be called after changing the others for the change to take place immediately.

See Window styles for more information about flags.

See: `getWindowStyleFlag/1`

# `setWindowVariant`

```erlang
-spec setWindowVariant(This, Variant) -> ok when This :: wxWindow(), Variant :: wx:wx_enum().
```

Chooses a different variant of the window display to use.

Window variants currently just differ in size, as can be seen from ?wxWindowVariant
documentation. Under all platforms but macOS, this function does nothing more than change
the font used by the window. However under macOS it is implemented natively and selects
the appropriate variant of the native widget, which has better appearance than just scaled
down or up version of the normal variant, so it should be preferred to directly tweaking
the font size.

By default the controls naturally use the normal variant.

# `shouldInheritColours`

```erlang
-spec shouldInheritColours(This) -> boolean() when This :: wxWindow().
```

Return true from here to allow the colours of this window to be changed by `inheritAttributes/1`.

Returning false forbids inheriting them from the parent window.

The base class version returns false, but this method is overridden in `m:wxControl`
where it returns true.

# `show`

```erlang
-spec show(This) -> boolean() when This :: wxWindow().
```

# `show`

```erlang
-spec show(This, [Option]) -> boolean() when This :: wxWindow(), Option :: {show, boolean()}.
```

Shows or hides the window.

You may need to call `raise/1` for a top level window if you want to bring it to top, although
this is not needed if `show/2` is called immediately after the frame creation.

Notice that the default state of newly created top level windows is hidden (to allow you
to create their contents without flicker) unlike for all the other, not derived from `m:wxTopLevelWindow`,
windows that are by default created in the shown state.

Return: true if the window has been shown or hidden or false if nothing was done because
it already was in the requested state.

See:
* `isShown/1`

* `hide/1`

* `wxRadioBox:show/3`

* `m:wxShowEvent`

# `thaw`

```erlang
-spec thaw(This) -> ok when This :: wxWindow().
```

Re-enables window updating after a previous call to `freeze/1`.

To really thaw the control, it must be called exactly the same number of times as `freeze/1`.

If the window has any children, they are recursively thawed too.

See:
* `freeze/1`

* `isFrozen/1`

# `toDIP`

```erlang
-spec toDIP(D, W) -> integer() when D :: integer(), W :: wxWindow();
           (Sz, W) -> {W :: integer(), H :: integer()}
               when Sz :: {W :: integer(), H :: integer()}, W :: wxWindow();
           (This, D) -> integer() when This :: wxWindow(), D :: integer();
           (This, Sz) -> {W :: integer(), H :: integer()}
               when This :: wxWindow(), Sz :: {W :: integer(), H :: integer()}.
```

Convert pixel values of the current toolkit to DPI-independent pixel values.

A DPI-independent pixel is just a pixel at the standard 96 DPI resolution. To keep the
same physical size at higher resolution, the physical pixel value must be scaled by `getDPIScaleFactor/1` but
this scaling may be already done by the underlying toolkit (GTK+, Cocoa, ...)
automatically. This method performs the conversion only if it is not already done by the
lower level toolkit, For example, you may want to use this to store window sizes and
positions so that they can be re-used regardless of the display DPI:

Also note that if either component of `sz` has the special value of -1, it is returned
unchanged independently of the current DPI, to preserve the special value of -1 in
wxWidgets API (it is often used to mean "unspecified").

Since: 3.1.0

# `transferDataFromWindow`

```erlang
-spec transferDataFromWindow(This) -> boolean() when This :: wxWindow().
```

Transfers values from child controls to data areas specified by their validators.

Returns false if a transfer failed.

Notice that this also calls `transferDataFromWindow/1` for all children recursively.

See:
* `transferDataToWindow/1`

* `validate/1`

# `transferDataToWindow`

```erlang
-spec transferDataToWindow(This) -> boolean() when This :: wxWindow().
```

Transfers values to child controls from data areas specified by their validators.

Notice that this also calls `transferDataToWindow/1` for all children recursively.

Return: Returns false if a transfer failed.

See:
* `transferDataFromWindow/1`

* `validate/1`

# `update`

```erlang
-spec update(This) -> ok when This :: wxWindow().
```

Calling this method immediately repaints the invalidated area of the window and all of
its children recursively (this normally only happens when the flow of control returns to
the event loop).

Notice that this function doesn't invalidate any area of the window so nothing happens if
nothing has been invalidated (i.e. marked as requiring a redraw). Use `refresh/2` first if you want
to immediately redraw the window unconditionally.

# `updateWindowUI`

```erlang
-spec updateWindowUI(This) -> ok when This :: wxWindow().
```

# `updateWindowUI`

```erlang
-spec updateWindowUI(This, [Option]) -> ok when This :: wxWindow(), Option :: {flags, integer()}.
```

This function sends one or more `m:wxUpdateUIEvent` to the window.

The particular implementation depends on the window; for example a `m:wxToolBar` will
send an update UI event for each toolbar button, and a `m:wxFrame` will send an update UI
event for each menubar menu item.

You can call this function from your application to ensure that your UI is up-to-date at
this point (as far as your `m:wxUpdateUIEvent` handlers are concerned). This may be
necessary if you have called `wxUpdateUIEvent:setMode/1` or `wxUpdateUIEvent:setUpdateInterval/1` to limit the overhead that wxWidgets incurs by sending
update UI events in idle time. `flags` should be a bitlist of one or more of the
?wxUpdateUI enumeration.

If you are calling this function from an OnInternalIdle or OnIdle function, make sure you
pass the wxUPDATE_UI_FROMIDLE flag, since this tells the window to only update the UI
elements that need to be updated in idle time. Some windows update their elements only
when necessary, for example when a menu is about to be shown. The following is an example
of how to call UpdateWindowUI from an idle function.

See: `m:wxUpdateUIEvent`

# `validate`

```erlang
-spec validate(This) -> boolean() when This :: wxWindow().
```

Validates the current values of the child controls using their validators.

Notice that this also calls `validate/1` for all children recursively.

Return: Returns false if any of the validations failed.

See:
* `transferDataFromWindow/1`

* `transferDataToWindow/1`

# `warpPointer`

```erlang
-spec warpPointer(This, X, Y) -> ok when This :: wxWindow(), X :: integer(), Y :: integer().
```

Moves the pointer to the given position on the window.

Note: Apple Human Interface Guidelines forbid moving the mouse cursor programmatically so
you should avoid using this function in Mac applications (and probably avoid using it
under the other platforms without good reason as well).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
