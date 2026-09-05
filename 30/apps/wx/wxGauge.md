# `wxGauge`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGauge.erl#L58)

A gauge is a horizontal or vertical bar which shows a quantity (often time).

`m:wxGauge` supports two working modes: determinate and indeterminate progress.

The first is the usual working mode (see `setValue/2` and `setRange/2`) while the second can be used when the
program is doing some processing but you don't know how much progress is being done. In
this case, you can periodically call the `pulse/1` function to make the progress bar switch to
indeterminate mode (graphically it's usually a set of blocks which move or bounce in the
bar control).

`m:wxGauge` supports dynamic switch between these two work modes.

There are no user commands for the gauge.

## Styles

This class supports the following styles:

* wxGA_HORIZONTAL: Creates a horizontal gauge.

* wxGA_VERTICAL: Creates a vertical gauge.

* wxGA_SMOOTH: Creates smooth progress bar with one pixel wide update step (not supported
by all platforms).

* wxGA_TEXT: Display the current value in percents in the gauge itself. This style is only
supported in wxQt and ignored under the other platforms. This flag is only available in
wxWidgets 3.1.0 and later.

* wxGA_PROGRESS: Reflect the value of gauge in the application taskbar button under Windows
7 and later and the dock icon under macOS, ignored under the other platforms. This flag is
only available in wxWidgets 3.1.0 and later.

See:
* `m:wxSlider`

* `m:wxScrollBar`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxGauge](https://docs.wxwidgets.org/3.2/classwx_gauge.html)

# `wxGauge`

```erlang
-type wxGauge() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Range) -> boolean()
                when
                    This :: wxGauge(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Range :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, Range, [Option]) -> boolean()
                when
                    This :: wxGauge(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Range :: integer(),
                    Option ::
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the gauge for two-step construction.

See `new/4` for further details.

# `destroy`

```erlang
-spec destroy(This :: wxGauge()) -> ok.
```

Destroys the object

# `getRange`

```erlang
-spec getRange(This) -> integer() when This :: wxGauge().
```

Returns the maximum position of the gauge.

See: `setRange/2`

# `getValue`

```erlang
-spec getValue(This) -> integer() when This :: wxGauge().
```

Returns the current position of the gauge.

See: `setValue/2`

# `isVertical`

```erlang
-spec isVertical(This) -> boolean() when This :: wxGauge().
```

Returns true if the gauge is vertical (has `wxGA_VERTICAL` style) and false otherwise.

# `new`

```erlang
-spec new() -> wxGauge().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id, Range) -> wxGauge()
             when Parent :: wxWindow:wxWindow(), Id :: integer(), Range :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, Range, [Option]) -> wxGauge()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Range :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a gauge.

See: `create/5`

# `pulse`

```erlang
-spec pulse(This) -> ok when This :: wxGauge().
```

Switch the gauge to indeterminate mode (if required) and makes the gauge move a bit to
indicate the user that some progress has been made.

Note: After calling this function the value returned by `getValue/1` is undefined and thus you need
to explicitly call `setValue/2` if you want to restore the determinate mode.

# `setRange`

```erlang
-spec setRange(This, Range) -> ok when This :: wxGauge(), Range :: integer().
```

Sets the range (maximum value) of the gauge.

This function makes the gauge switch to determinate mode, if it's not already.

When the gauge is in indeterminate mode, under wxMSW the gauge repeatedly goes from zero
to `range` and back; under other ports when in indeterminate mode, the `range` setting is ignored.

See: `getRange/1`

# `setValue`

```erlang
-spec setValue(This, Pos) -> ok when This :: wxGauge(), Pos :: integer().
```

Sets the position of the gauge.

The `pos` must be between 0 and the gauge range as returned by `getRange/1`, inclusive.

This function makes the gauge switch to determinate mode, if it was in indeterminate mode before.

See: `getValue/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
