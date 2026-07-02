# `wxSlider`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxSlider.erl#L58)

A slider is a control with a handle which can be pulled back and forth to change the
value.

On Windows, the track bar control is used.

On GTK+, tick marks are only available for version 2.16 and later.

Slider generates the same events as `m:wxScrollBar` but in practice the most convenient
way to process `m:wxSlider` updates is by handling the slider-specific `wxEVT_SLIDER`
event which carries `m:wxCommandEvent` containing just the latest slider position.

## Styles

This class supports the following styles:

* wxSL_HORIZONTAL: Displays the slider horizontally (this is the default).

* wxSL_VERTICAL: Displays the slider vertically.

* wxSL_AUTOTICKS: Displays tick marks (Windows, GTK+ 2.16 and later).

* wxSL_MIN_MAX_LABELS: Displays minimum, maximum labels (new since wxWidgets 2.9.1).

* wxSL_VALUE_LABEL: Displays value label (new since wxWidgets 2.9.1).

* wxSL_LABELS: Displays minimum, maximum and value labels (same as wxSL_VALUE_LABEL and
wxSL_MIN_MAX_LABELS together).

* wxSL_LEFT: Displays ticks on the left and forces the slider to be vertical (Windows and
GTK+ 3 only).

* wxSL_RIGHT: Displays ticks on the right and forces the slider to be vertical.

* wxSL_TOP: Displays ticks on the top (Windows and GTK+ 3 only).

* wxSL_BOTTOM: Displays ticks on the bottom (this is the default).

* wxSL_BOTH: Displays ticks on both sides of the slider. Windows only.

* wxSL_SELRANGE: Displays a highlighted selection range. Windows only.

* wxSL_INVERSE: Inverses the minimum and maximum endpoints on the slider. Not compatible
with wxSL_SELRANGE. Notice that `wxSL_LEFT`, `wxSL_TOP`, `wxSL_RIGHT` and `wxSL_BOTTOM`
specify the position of the slider ticks and that the slider labels, if any, are
positioned on the opposite side. So, to have a label on the left side of a vertical
slider, `wxSL_RIGHT` must be used (or none of these styles at all should be specified as
left and top are default positions for the vertical and horizontal sliders respectively).

The difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED

The EVT_SCROLL_THUMBRELEASE event is only emitted when actually dragging the thumb using
the mouse and releasing it (This EVT_SCROLL_THUMBRELEASE event is also followed by an
EVT_SCROLL_CHANGED event).

The EVT_SCROLL_CHANGED event also occurs when using the keyboard to change the thumb
position, and when clicking next to the thumb (In all these cases the
EVT_SCROLL_THUMBRELEASE event does not happen). In short, the EVT_SCROLL_CHANGED event is
triggered when scrolling/ moving has finished independently of the way it had started.
Please see the page_samples_widgets ("Slider" page) to see the difference between
EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED in action.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxScrollBar`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSlider](https://docs.wxwidgets.org/3.2/classwx_slider.html)

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

* [`command_slider_updated`](`m:wxCommandEvent`)

# `wxSlider`

```elixir
-type wxSlider() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Value, MinValue, MaxValue) -> boolean()
                when
                    This :: wxSlider(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Value :: integer(),
                    MinValue :: integer(),
                    MaxValue :: integer().
```

# `create`

```elixir
-spec create(This, Parent, Id, Value, MinValue, MaxValue, [Option]) -> boolean()
                when
                    This :: wxSlider(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Value :: integer(),
                    MinValue :: integer(),
                    MaxValue :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Used for two-step slider construction.

See `new/6` for further details.

# `destroy`

```elixir
-spec destroy(This :: wxSlider()) -> ok.
```

Destroys the object

# `getLineSize`

```elixir
-spec getLineSize(This) -> integer() when This :: wxSlider().
```

Returns the line size.

See: `setLineSize/2`

# `getMax`

```elixir
-spec getMax(This) -> integer() when This :: wxSlider().
```

Gets the maximum slider value.

See:
* `getMin/1`

* `setRange/3`

# `getMin`

```elixir
-spec getMin(This) -> integer() when This :: wxSlider().
```

Gets the minimum slider value.

See:
* `getMin/1`

* `setRange/3`

# `getPageSize`

```elixir
-spec getPageSize(This) -> integer() when This :: wxSlider().
```

Returns the page size.

See: `setPageSize/2`

# `getThumbLength`

```elixir
-spec getThumbLength(This) -> integer() when This :: wxSlider().
```

Returns the thumb length.

Only for:wxmsw

See: `setThumbLength/2`

# `getValue`

```elixir
-spec getValue(This) -> integer() when This :: wxSlider().
```

Gets the current slider value.

See:
* `getMin/1`

* `getMax/1`

* `setValue/2`

# `new`

```elixir
-spec new() -> wxSlider().
```

Default constructor.

# `new`

```elixir
-spec new(Parent, Id, Value, MinValue, MaxValue) -> wxSlider()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Value :: integer(),
                 MinValue :: integer(),
                 MaxValue :: integer().
```

# `new`

```elixir
-spec new(Parent, Id, Value, MinValue, MaxValue, [Option]) -> wxSlider()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Value :: integer(),
                 MinValue :: integer(),
                 MaxValue :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a slider.

See: `create/7`

# `setLineSize`

```elixir
-spec setLineSize(This, LineSize) -> ok when This :: wxSlider(), LineSize :: integer().
```

Sets the line size for the slider.

See: `getLineSize/1`

# `setPageSize`

```elixir
-spec setPageSize(This, PageSize) -> ok when This :: wxSlider(), PageSize :: integer().
```

Sets the page size for the slider.

See: `getPageSize/1`

# `setRange`

```elixir
-spec setRange(This, MinValue, MaxValue) -> ok
                  when This :: wxSlider(), MinValue :: integer(), MaxValue :: integer().
```

Sets the minimum and maximum slider values.

See:
* `getMin/1`

* `getMax/1`

# `setThumbLength`

```elixir
-spec setThumbLength(This, Len) -> ok when This :: wxSlider(), Len :: integer().
```

Sets the slider thumb length.

Only for:wxmsw

See: `getThumbLength/1`

# `setValue`

```elixir
-spec setValue(This, Value) -> ok when This :: wxSlider(), Value :: integer().
```

Sets the slider position.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
