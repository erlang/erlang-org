# `wxScrolledWindow`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxScrolledWindow.erl#L58)

The `wxScrolled` (not implemented in wx) class manages scrolling for its client area,
transforming the coordinates according to the scrollbar positions, and setting the scroll
positions, thumb sizes and ranges according to the area in view.

There are two commonly used (but not the only possible!) specializations of this class:

* ?wxScrolledWindow, aka wxScrolled<wxPanel>, is equivalent to ?wxScrolledWindow from
earlier versions. Derived from `m:wxPanel`, it shares `m:wxPanel`'s behaviour with regard
to TAB traversal and focus handling. Use this if the scrolled window will have child controls.

* ?wxScrolledCanvas, aka wxScrolled<wxWindow>, derives from `m:wxWindow` and so doesn't
handle children specially. This is suitable e.g. for implementing scrollable controls such
as tree or list controls.

Note: See `wxScrolled::Create()` (not implemented in wx) if you want to use `wxScrolled`
(not implemented in wx) with a custom class.

Starting from version 2.4 of wxWidgets, there are several ways to use a
?wxScrolledWindow (and now `wxScrolled` (not implemented in wx)). In particular, there are
three ways to set the size of the scrolling area:

One way is to set the scrollbars directly using a call to `setScrollbars/6`. This is the way it used to be
in any previous version of wxWidgets and it will be kept for backwards compatibility.

An additional method of manual control, which requires a little less computation of your
own, is to set the total size of the scrolling area by calling either `wxWindow:setVirtualSize/3`, or `wxWindow:fitInside/1`, and setting
the scrolling increments for it by calling `setScrollRate/3`. Scrolling in some orientation is enabled by
setting a non-zero increment for it.

The most automatic and newest way is to simply let sizers determine the scrolling area.
This is now the default when you set an interior sizer into a `wxScrolled` (not
implemented in wx) with `wxWindow:setSizer/3`. The scrolling area will be set to the size requested by the
sizer and the scrollbars will be assigned for each orientation according to the need for
them and the scrolling increment set by `setScrollRate/3`. As above, scrolling is only enabled in
orientations with a non-zero increment. You can influence the minimum size of the scrolled
area controlled by a sizer by calling wxWindow::SetVirtualSizeHints(). (Calling `setScrollbars/6` has
analogous effects in wxWidgets 2.4 - in later versions it may not continue to override the sizer.)

Note that if maximum size hints are still supported by wxWindow::SetVirtualSizeHints(),
use them at your own dire risk. They may or may not have been removed for 2.4, but it
really only makes sense to set minimum size hints here. We should probably replace
wxWindow::SetVirtualSizeHints() with wxWindow::SetMinVirtualSize() or similar and remove
it entirely in future.

As with all windows, an application can draw onto a `wxScrolled` (not implemented in wx)
using a device context.

You have the option of handling the OnPaint handler or overriding the `wxScrolled::OnDraw()`
(not implemented in wx) function, which is passed a pre-scrolled device context (prepared
by `doPrepareDC/2`).

If you don't wish to calculate your own scrolling, you must call `doPrepareDC/2` when not drawing from
within `OnDraw()` (not implemented in wx), to set the device origin for the device context
according to the current scroll position.

A `wxScrolled` (not implemented in wx) will normally scroll itself and therefore its
child windows as well. It might however be desired to scroll a different window than
itself: e.g. when designing a spreadsheet, you will normally only have to scroll the
(usually white) cell area, whereas the (usually grey) label area will scroll very
differently. For this special purpose, you can call `setTargetWindow/2` which means that pressing the
scrollbars will scroll a different window.

Note that the underlying system knows nothing about scrolling coordinates, so that all
system functions (mouse events, expose events, refresh calls etc) as well as the position
of subwindows are relative to the "physical" origin of the scrolled window. If the user
insert a child window at position (10,10) and scrolls the window down 100 pixels (moving
the child window out of the visible area), the child window will report a position of (10,-90).

## Styles

This class supports the following styles:

* wxHSCROLL: If this style is specified and ?wxVSCROLL isn't, the window will be scrollable
only in horizontal direction (by default, i.e. if neither this style nor ?wxVSCROLL is
specified, it scrolls in both directions).

* wxVSCROLL: If this style is specified and ?wxHSCROLL isn't, the window will be scrollable
only in vertical direction (by default, i.e. if neither this style nor ?wxHSCROLL is
specified, it scrolls in both directions).

* wxALWAYS_SHOW_SB: Since wxWidgets 2.9.5, specifying this style makes the window always
show its scrollbars, even if they are not used. See `ShowScrollbars()` (not implemented in
wx).

* wxRETAINED: Uses a backing pixmap to speed refreshes. Motif only.

See:
* `m:wxScrollBar`

* `m:wxClientDC`

* `m:wxPaintDC`

This class is derived, and can use functions, from:

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxScrolledWindow](https://docs.wxwidgets.org/3.2/classwx_scrolled_window.html)

## Events

Event types emitted from this class:

* [`scrollwin_top`](`m:wxScrollWinEvent`)

* [`scrollwin_bottom`](`m:wxScrollWinEvent`)

* [`scrollwin_lineup`](`m:wxScrollWinEvent`)

* [`scrollwin_linedown`](`m:wxScrollWinEvent`)

* [`scrollwin_pageup`](`m:wxScrollWinEvent`)

* [`scrollwin_pagedown`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbtrack`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbrelease`](`m:wxScrollWinEvent`)

# `wxScrolledWindow`

```erlang
-type wxScrolledWindow() :: wx:wx_object().
```

# `calcScrolledPosition`

```erlang
-spec calcScrolledPosition(This, Pt) -> {X :: integer(), Y :: integer()}
                              when This :: wxScrolledWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

# `calcScrolledPosition`

```erlang
-spec calcScrolledPosition(This, X, Y) -> {Xx :: integer(), Yy :: integer()}
                              when This :: wxScrolledWindow(), X :: integer(), Y :: integer().
```

Translates the logical coordinates to the device ones.

For example, if a window is scrolled 10 pixels to the bottom, the device coordinates of
the origin are (0, 0) (as always), but the logical coordinates are (0, 10) and so the call
to CalcScrolledPosition(0, 10, xx, yy) will return 0 in yy.

See: `calcUnscrolledPosition/3`

# `calcUnscrolledPosition`

```erlang
-spec calcUnscrolledPosition(This, Pt) -> {X :: integer(), Y :: integer()}
                                when This :: wxScrolledWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

# `calcUnscrolledPosition`

```erlang
-spec calcUnscrolledPosition(This, X, Y) -> {Xx :: integer(), Yy :: integer()}
                                when This :: wxScrolledWindow(), X :: integer(), Y :: integer().
```

Translates the device coordinates to the logical ones.

For example, if a window is scrolled 10 pixels to the bottom, the device coordinates of
the origin are (0, 0) (as always), but the logical coordinates are (0, 10) and so the call
to CalcUnscrolledPosition(0, 0, xx, yy) will return 10 in yy.

See: `calcScrolledPosition/3`

# `destroy`

```erlang
-spec destroy(This :: wxScrolledWindow()) -> ok.
```

Destroys the object

# `doPrepareDC`

```erlang
-spec doPrepareDC(This, Dc) -> ok when This :: wxScrolledWindow(), Dc :: wxDC:wxDC().
```

Call this function to prepare the device context for drawing a scrolled image.

It sets the device origin according to the current scroll position. `doPrepareDC/2` is called
automatically within the default `wxEVT_PAINT` event handler, so your `OnDraw()` (not
implemented in wx) override will be passed an already 'pre-scrolled' device context.
However, if you wish to draw from outside of `OnDraw()` (not implemented in wx) (e.g. from
your own `wxEVT_PAINT` handler), you must call this function yourself.

For example:

Notice that the function sets the origin by moving it relatively to the current origin
position, so you shouldn't change the origin before calling `doPrepareDC/2` or, if you do, reset it to
(0, 0) later. If you call `doPrepareDC/2` immediately after device context creation, as in the example
above, this problem doesn't arise, of course, so it is customary to do it like this.

# `enableScrolling`

```erlang
-spec enableScrolling(This, XScrolling, YScrolling) -> ok
                         when
                             This :: wxScrolledWindow(),
                             XScrolling :: boolean(),
                             YScrolling :: boolean().
```

Enable or disable use of `wxWindow:scrollWindow/4` for scrolling.

By default, when a scrolled window is logically scrolled, `wxWindow:scrollWindow/4` is called on the underlying
window which scrolls the window contents and only invalidates the part of the window newly
brought into view. If false is passed as an argument, then this "physical scrolling" is
disabled and the window is entirely invalidated whenever it is scrolled by calling `wxWindow:refresh/2`.

It should be rarely necessary to disable physical scrolling, so this method shouldn't be
called in normal circumstances.

# `getScrollPixelsPerUnit`

```erlang
-spec getScrollPixelsPerUnit(This) -> {XUnit :: integer(), YUnit :: integer()}
                                when This :: wxScrolledWindow().
```

Get the number of pixels per scroll unit (line), in each direction, as set by `setScrollbars/6`.

A value of zero indicates no scrolling in that direction.

See:
* `setScrollbars/6`

* `wxWindow:getVirtualSize/1`

# `getViewStart`

```erlang
-spec getViewStart(This) -> {X :: integer(), Y :: integer()} when This :: wxScrolledWindow().
```

This is a simple overload of GetViewStart(int*,int*); see that function for more info.

# `new`

```erlang
-spec new() -> wxScrolledWindow().
```

Default constructor.

# `new`

```erlang
-spec new(Parent) -> wxScrolledWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxScrolledWindow()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {winid, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Constructor.

Remark: The window is initially created without visible scrollbars. Call `setScrollbars/6` to specify how
big the virtual window size should be.

# `prepareDC`

```erlang
-spec prepareDC(This, Dc) -> ok when This :: wxScrolledWindow(), Dc :: wxDC:wxDC().
```

This function is for backwards compatibility only and simply calls `doPrepareDC/2` now.

Notice that it is not called by the default paint event handle (`doPrepareDC/2` is), so overriding this
method in your derived class is useless.

# `scroll`

```erlang
-spec scroll(This, Pt) -> ok when This :: wxScrolledWindow(), Pt :: {X :: integer(), Y :: integer()}.
```

This is an overload of `scroll/3`;see that function for more info.

# `scroll`

```erlang
-spec scroll(This, X, Y) -> ok when This :: wxScrolledWindow(), X :: integer(), Y :: integer().
```

Scrolls a window so the view start is at the given point.

Remark: The positions are in scroll units, not pixels, so to convert to pixels you will
have to multiply by the number of pixels per scroll increment. If either parameter is
?wxDefaultCoord (-1), that position will be ignored (no change in that direction).

See:
* `setScrollbars/6`

* `getScrollPixelsPerUnit/1`

# `setScrollbars`

```erlang
-spec setScrollbars(This, PixelsPerUnitX, PixelsPerUnitY, NoUnitsX, NoUnitsY) -> ok
                       when
                           This :: wxScrolledWindow(),
                           PixelsPerUnitX :: integer(),
                           PixelsPerUnitY :: integer(),
                           NoUnitsX :: integer(),
                           NoUnitsY :: integer().
```

# `setScrollbars`

```erlang
-spec setScrollbars(This, PixelsPerUnitX, PixelsPerUnitY, NoUnitsX, NoUnitsY, [Option]) -> ok
                       when
                           This :: wxScrolledWindow(),
                           PixelsPerUnitX :: integer(),
                           PixelsPerUnitY :: integer(),
                           NoUnitsX :: integer(),
                           NoUnitsY :: integer(),
                           Option :: {xPos, integer()} | {yPos, integer()} | {noRefresh, boolean()}.
```

Sets up vertical and/or horizontal scrollbars.

The first pair of parameters give the number of pixels per 'scroll step', i.e. amount
moved when the up or down scroll arrows are pressed. The second pair gives the length of
scrollbar in scroll steps, which sets the size of the virtual window.

`xPos` and `yPos` optionally specify a position to scroll to immediately.

For example, the following gives a window horizontal and vertical scrollbars with 20
pixels per scroll step, and a size of 50 steps (1000 pixels) in each direction:

`wxScrolled` (not implemented in wx) manages the page size itself, using the current
client window size as the page size.

Note that for more sophisticated scrolling applications, for example where scroll steps
may be variable according to the position in the document, it will be necessary to derive
a new class from `m:wxWindow`, overriding OnSize() and adjusting the scrollbars appropriately.

See: `wxWindow:setVirtualSize/3`

# `setScrollRate`

```erlang
-spec setScrollRate(This, Xstep, Ystep) -> ok
                       when This :: wxScrolledWindow(), Xstep :: integer(), Ystep :: integer().
```

Set the horizontal and vertical scrolling increment only.

See the pixelsPerUnit parameter in `setScrollbars/6`.

# `setTargetWindow`

```erlang
-spec setTargetWindow(This, Window) -> ok when This :: wxScrolledWindow(), Window :: wxWindow:wxWindow().
```

Call this function to tell `wxScrolled` (not implemented in wx) to perform the actual
scrolling on a different window (and not on itself).

This method is useful when only a part of the window should be scrolled. A typical
example is a control consisting of a fixed header and the scrollable contents window: the
scrollbars are attached to the main window itself, hence it, and not the contents window
must be derived from `wxScrolled` (not implemented in wx), but only the contents window
scrolls when the scrollbars are used. To implement such setup, you need to call this
method with the contents window as argument.

Notice that if this method is used, `GetSizeAvailableForScrollTarget()` (not implemented
in wx) method must be overridden.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
