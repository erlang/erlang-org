# `wxSplitterWindow`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxSplitterWindow.erl#L58)

This class manages up to two subwindows.

The current view can be split into two programmatically (perhaps from a menu command),
and unsplit either programmatically or via the `m:wxSplitterWindow` user interface.

## Styles

This class supports the following styles:

* wxSP_3D: Draws a 3D effect border and sash.

* wxSP_THIN_SASH: Draws a thin sash.

* wxSP_3DSASH: Draws a 3D effect sash (part of default style).

* wxSP_3DBORDER: Synonym for wxSP_BORDER.

* wxSP_BORDER: Draws a standard border.

* wxSP_NOBORDER: No border (default).

* wxSP_NO_XP_THEME: Under Windows, switches off the attempt to draw the splitter using
Windows theming, so the borders and sash will take on the pre-XP look.

* wxSP_PERMIT_UNSPLIT: Always allow to unsplit, even with the minimum pane size other than
zero.

* wxSP_LIVE_UPDATE: Don't draw XOR line but resize the child windows immediately.

See:
* `m:wxSplitterEvent`

* [Overview splitterwindow](https://docs.wxwidgets.org/3.2/overview_splitterwindow.html#overview_splitterwindow)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSplitterWindow](https://docs.wxwidgets.org/3.2/classwx_splitter_window.html)

## Events

Event types emitted from this class:

* [`command_splitter_sash_pos_changing`](`m:wxSplitterEvent`)

* [`command_splitter_sash_pos_changed`](`m:wxSplitterEvent`)

* [`command_splitter_unsplit`](`m:wxSplitterEvent`)

# `wxSplitterWindow`

```erlang
-type wxSplitterWindow() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxSplitterWindow(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxSplitterWindow(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creation function, for two-step construction.

See `new/2` for details.

# `destroy`

```erlang
-spec destroy(This :: wxSplitterWindow()) -> ok.
```

Destroys the object

# `getMinimumPaneSize`

```erlang
-spec getMinimumPaneSize(This) -> integer() when This :: wxSplitterWindow().
```

Returns the current minimum pane size (defaults to zero).

See: `setMinimumPaneSize/2`

# `getSashGravity`

```erlang
-spec getSashGravity(This) -> number() when This :: wxSplitterWindow().
```

Returns the current sash gravity.

See: `setSashGravity/2`

# `getSashPosition`

```erlang
-spec getSashPosition(This) -> integer() when This :: wxSplitterWindow().
```

Returns the current sash position.

See: `setSashPosition/3`

# `getSplitMode`

```erlang
-spec getSplitMode(This) -> wx:wx_enum() when This :: wxSplitterWindow().
```

Gets the split mode.

See:
* `setSplitMode/2`

* `splitVertically/4`

* `splitHorizontally/4`

# `getWindow1`

```erlang
-spec getWindow1(This) -> wxWindow:wxWindow() when This :: wxSplitterWindow().
```

Returns the left/top or only pane.

# `getWindow2`

```erlang
-spec getWindow2(This) -> wxWindow:wxWindow() when This :: wxSplitterWindow().
```

Returns the right/bottom pane.

# `initialize`

```erlang
-spec initialize(This, Window) -> ok when This :: wxSplitterWindow(), Window :: wxWindow:wxWindow().
```

Initializes the splitter window to have one pane.

The child window is shown if it is currently hidden.

Remark: This should be called if you wish to initially view only a single pane in the
splitter window.

See:
* `splitVertically/4`

* `splitHorizontally/4`

# `isSplit`

```erlang
-spec isSplit(This) -> boolean() when This :: wxSplitterWindow().
```

Returns true if the window is split, false otherwise.

# `new`

```erlang
-spec new() -> wxSplitterWindow().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxSplitterWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxSplitterWindow()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor for creating the window.

Remark: After using this constructor, you must create either one or two subwindows with
the splitter window as parent, and then call one of `initialize/2`, `splitVertically/4` and `splitHorizontally/4` in order to set the pane(s).
You can create two windows, with one hidden when not being shown; or you can create and
delete the second pane on demand.

See:
* `initialize/2`

* `splitVertically/4`

* `splitHorizontally/4`

* `create/3`

# `replaceWindow`

```erlang
-spec replaceWindow(This, WinOld, WinNew) -> boolean()
                       when
                           This :: wxSplitterWindow(),
                           WinOld :: wxWindow:wxWindow(),
                           WinNew :: wxWindow:wxWindow().
```

This function replaces one of the windows managed by the `m:wxSplitterWindow` with
another one.

It is in general better to use it instead of calling `unsplit/2` and then resplitting the window
back because it will provoke much less flicker (if any). It is valid to call this function
whether the splitter has two windows or only one.

Both parameters should be non-NULL and `winOld` must specify one of the windows managed
by the splitter. If the parameters are incorrect or the window couldn't be replaced, false
is returned. Otherwise the function will return true, but please notice that it will not
delete the replaced window and you may wish to do it yourself.

See: `getMinimumPaneSize/1`

# `setMinimumPaneSize`

```erlang
-spec setMinimumPaneSize(This, PaneSize) -> ok when This :: wxSplitterWindow(), PaneSize :: integer().
```

Sets the minimum pane size.

Remark: The default minimum pane size is zero, which means that either pane can be
reduced to zero by dragging the sash, thus removing one of the panes. To prevent this
behaviour (and veto out-of-range sash dragging), set a minimum size, for example 20
pixels. If the wxSP_PERMIT_UNSPLIT style is used when a splitter window is created, the
window may be unsplit even if minimum size is non-zero.

See: `getMinimumPaneSize/1`

# `setSashGravity`

```erlang
-spec setSashGravity(This, Gravity) -> ok when This :: wxSplitterWindow(), Gravity :: number().
```

Sets the sash gravity.

Remark: Gravity is real factor which controls position of sash while resizing `m:wxSplitterWindow`.
Gravity tells `m:wxSplitterWindow` how much will left/top window grow while resizing.
Example values:

* 0.0: only the bottom/right window is automatically resized

* 0.5: both windows grow by equal size

* 1.0: only left/top window grows Gravity should be a real value between 0.0 and 1.0.
Default value of sash gravity is 0.0. That value is compatible with previous (before
gravity was introduced) behaviour of `m:wxSplitterWindow`.

Notice that when sash gravity for a newly created splitter window, it is often necessary
to explicitly set the splitter size using `wxWindow:setSize/6` to ensure that is big enough for its initial
sash position. Otherwise, i.e. if the window is created with the default tiny size and
only resized to its correct size later, the initial sash position will be affected by the
gravity and typically result in sash being at the rightmost position for the gravity of 1.
See the example code creating `m:wxSplitterWindow` in the splitter sample for more details.

See: `getSashGravity/1`

# `setSashPosition`

```erlang
-spec setSashPosition(This, Position) -> ok when This :: wxSplitterWindow(), Position :: integer().
```

# `setSashPosition`

```erlang
-spec setSashPosition(This, Position, [Option]) -> ok
                         when
                             This :: wxSplitterWindow(),
                             Position :: integer(),
                             Option :: {redraw, boolean()}.
```

Sets the sash position.

Remark: Does not currently check for an out-of-range value.

See: `getSashPosition/1`

# `setSplitMode`

```erlang
-spec setSplitMode(This, Mode) -> ok when This :: wxSplitterWindow(), Mode :: integer().
```

Sets the split mode.

Remark: Only sets the internal variable; does not update the display.

See:
* `getSplitMode/1`

* `splitVertically/4`

* `splitHorizontally/4`

# `splitHorizontally`

```erlang
-spec splitHorizontally(This, Window1, Window2) -> boolean()
                           when
                               This :: wxSplitterWindow(),
                               Window1 :: wxWindow:wxWindow(),
                               Window2 :: wxWindow:wxWindow().
```

# `splitHorizontally`

```erlang
-spec splitHorizontally(This, Window1, Window2, [Option]) -> boolean()
                           when
                               This :: wxSplitterWindow(),
                               Window1 :: wxWindow:wxWindow(),
                               Window2 :: wxWindow:wxWindow(),
                               Option :: {sashPosition, integer()}.
```

Initializes the top and bottom panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can also be
called at any subsequent time, but the application should check that the window is not
currently split using `isSplit/1`.

See:
* `splitVertically/4`

* `isSplit/1`

* `unsplit/2`

# `splitVertically`

```erlang
-spec splitVertically(This, Window1, Window2) -> boolean()
                         when
                             This :: wxSplitterWindow(),
                             Window1 :: wxWindow:wxWindow(),
                             Window2 :: wxWindow:wxWindow().
```

# `splitVertically`

```erlang
-spec splitVertically(This, Window1, Window2, [Option]) -> boolean()
                         when
                             This :: wxSplitterWindow(),
                             Window1 :: wxWindow:wxWindow(),
                             Window2 :: wxWindow:wxWindow(),
                             Option :: {sashPosition, integer()}.
```

Initializes the left and right panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can also be
called at any subsequent time, but the application should check that the window is not
currently split using `isSplit/1`.

See:
* `splitHorizontally/4`

* `isSplit/1`

* `unsplit/2`

# `unsplit`

```erlang
-spec unsplit(This) -> boolean() when This :: wxSplitterWindow().
```

# `unsplit`

```erlang
-spec unsplit(This, [Option]) -> boolean()
                 when This :: wxSplitterWindow(), Option :: {toRemove, wxWindow:wxWindow()}.
```

Unsplits the window.

Return: true if successful, false otherwise (the window was not split).

Remark: This call will not actually delete the pane being removed; it calls `OnUnsplit()`
(not implemented in wx) which can be overridden for the desired behaviour. By default, the
pane being removed is hidden.

See:
* `splitHorizontally/4`

* `splitVertically/4`

* `isSplit/1`

# `updateSize`

```erlang
-spec updateSize(This) -> ok when This :: wxSplitterWindow().
```

Causes any pending sizing of the sash and child panes to take place immediately.

Such resizing normally takes place in idle time, in order to wait for layout to be
completed. However, this can cause unacceptable flicker as the panes are resized after the
window has been shown. To work around this, you can perform window layout (for example by
sending a size event to the parent window), and then call this function, before showing
the top-level window.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
