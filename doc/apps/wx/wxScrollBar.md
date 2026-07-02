# `wxScrollBar`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxScrollBar.erl#L58)

A `m:wxScrollBar` is a control that represents a horizontal or vertical scrollbar.

It is distinct from the two scrollbars that some windows provide automatically, but the
two types of scrollbar share the way events are received.

Remark: A scrollbar has the following main attributes: range, thumb size, page size, and
position. The range is the total number of units associated with the view represented by
the scrollbar. For a table with 15 columns, the range would be 15. The thumb size is the
number of units that are currently visible. For the table example, the window might be
sized so that only 5 columns are currently visible, in which case the application would
set the thumb size to 5. When the thumb size becomes the same as or greater than the
range, the scrollbar will be automatically hidden on most platforms. The page size is the
number of units that the scrollbar should scroll by, when 'paging' through the data. This
value is normally the same as the thumb size length, because it is natural to assume that
the visible window size defines a page. The scrollbar position is the current thumb
position. Most applications will find it convenient to provide a function called
AdjustScrollbars() which can be called initially, from an OnSize event handler, and
whenever the application data changes in size. It will adjust the view, object and page
size according to the size of the window and the size of the data.

## Styles

This class supports the following styles:

* wxSB_HORIZONTAL: Specifies a horizontal scrollbar.

* wxSB_VERTICAL: Specifies a vertical scrollbar.

The difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED

The EVT_SCROLL_THUMBRELEASE event is only emitted when actually dragging the thumb using
the mouse and releasing it (This EVT_SCROLL_THUMBRELEASE event is also followed by an
EVT_SCROLL_CHANGED event).

The EVT_SCROLL_CHANGED event also occurs when using the keyboard to change the thumb
position, and when clicking next to the thumb (In all these cases the
EVT_SCROLL_THUMBRELEASE event does not happen).

In short, the EVT_SCROLL_CHANGED event is triggered when scrolling/moving has finished
independently of the way it had started. Please see the page_samples_widgets ("Slider"
page) to see the difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED in action.

See:
* [Overview scrolling](https://docs.wxwidgets.org/3.2/overview_scrolling.html#overview_scrolling)

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxScrollBar](https://docs.wxwidgets.org/3.2/classwx_scroll_bar.html)

## Events

Event types emitted from this class:

* [`scroll_top`](`m:wxScrollEvent`)

* [`scroll_bottom`](`m:wxScrollEvent`)

* [`scroll_lineup`](`m:wxScrollEvent`)

* [`scroll_linedown`](`m:wxScrollEvent`)

* [`scroll_pageup`](`m:wxScrollEvent`)

* [`scroll_pagedown`](`m:wxScrollEvent`)

* [`scroll_thumbtrack`](`m:wxScrollEvent`)

* [`scroll_thumbrelease`](`m:wxScrollEvent`)

* [`scroll_changed`](`m:wxScrollEvent`)

* [`scroll_top`](`m:wxScrollEvent`)

* [`scroll_bottom`](`m:wxScrollEvent`)

* [`scroll_lineup`](`m:wxScrollEvent`)

* [`scroll_linedown`](`m:wxScrollEvent`)

* [`scroll_pageup`](`m:wxScrollEvent`)

* [`scroll_pagedown`](`m:wxScrollEvent`)

* [`scroll_thumbtrack`](`m:wxScrollEvent`)

* [`scroll_thumbrelease`](`m:wxScrollEvent`)

* [`scroll_changed`](`m:wxScrollEvent`)

# `wxScrollBar`

```erlang
-type wxScrollBar() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxScrollBar(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxScrollBar(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Scrollbar creation function called by the scrollbar constructor.

See `new/3` for details.

# `destroy`

```erlang
-spec destroy(This :: wxScrollBar()) -> ok.
```

Destroys the object

# `getPageSize`

```erlang
-spec getPageSize(This) -> integer() when This :: wxScrollBar().
```

Returns the page size of the scrollbar.

This is the number of scroll units that will be scrolled when the user pages up or down.
Often it is the same as the thumb size.

See: `setScrollbar/6`

# `getRange`

```erlang
-spec getRange(This) -> integer() when This :: wxScrollBar().
```

Returns the length of the scrollbar.

See: `setScrollbar/6`

# `getThumbPosition`

```erlang
-spec getThumbPosition(This) -> integer() when This :: wxScrollBar().
```

Returns the current position of the scrollbar thumb.

See: `setThumbPosition/2`

# `getThumbSize`

```erlang
-spec getThumbSize(This) -> integer() when This :: wxScrollBar().
```

Returns the thumb or 'view' size.

See: `setScrollbar/6`

# `new`

```erlang
-spec new() -> wxScrollBar().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxScrollBar() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxScrollBar()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a scrollbar.

See: `create/4`

# `setScrollbar`

```erlang
-spec setScrollbar(This, Position, ThumbSize, Range, PageSize) -> ok
                      when
                          This :: wxScrollBar(),
                          Position :: integer(),
                          ThumbSize :: integer(),
                          Range :: integer(),
                          PageSize :: integer().
```

# `setScrollbar`

```erlang
-spec setScrollbar(This, Position, ThumbSize, Range, PageSize, [Option]) -> ok
                      when
                          This :: wxScrollBar(),
                          Position :: integer(),
                          ThumbSize :: integer(),
                          Range :: integer(),
                          PageSize :: integer(),
                          Option :: {refresh, boolean()}.
```

Sets the scrollbar properties.

Remark: Let's say you wish to display 50 lines of text, using the same font. The window
is sized so that you can only see 16 lines at a time. You would use: The page size is 1
less than the thumb size so that the last line of the previous page will be visible on the
next page, to help orient the user. Note that with the window at this size, the thumb
position can never go above 50 minus 16, or 34. You can determine how many lines are
currently visible by dividing the current view size by the character height in pixels.
When defining your own scrollbar behaviour, you will always need to recalculate the
scrollbar settings when the window size changes. You could therefore put your scrollbar
calculations and `setScrollbar/6` call into a function named AdjustScrollbars, which can be called
initially and also from a `m:wxSizeEvent` event handler function.

# `setThumbPosition`

```erlang
-spec setThumbPosition(This, ViewStart) -> ok when This :: wxScrollBar(), ViewStart :: integer().
```

Sets the position of the scrollbar.

See: `getThumbPosition/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
