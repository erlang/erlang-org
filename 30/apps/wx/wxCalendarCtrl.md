# `wxCalendarCtrl`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCalendarCtrl.erl#L58)

The calendar control allows the user to pick a date.

The user can move the current selection using the keyboard and select the date
(generating `EVT_CALENDAR` event) by pressing `<Return>` or double clicking it.

Generic calendar has advanced possibilities for the customization of its display,
described below. If you want to use these possibilities on every platform, use
wxGenericCalendarCtrl instead of `m:wxCalendarCtrl`.

All global settings (such as colours and fonts used) can, of course, be changed. But
also, the display style for each day in the month can be set independently using `m:wxCalendarDateAttr`
class.

An item without custom attributes is drawn with the default colours and font and without
border, but setting custom attributes with `setAttr/3` allows modifying its appearance. Just create a
custom attribute object and set it for the day you want to be displayed specially (note
that the control will take ownership of the pointer, i.e. it will delete it itself). A day
may be marked as being a holiday, even if it is not recognized as one by `wx_datetime()` using the `wxCalendarDateAttr:setHoliday/2` method.

As the attributes are specified for each day, they may change when the month is changed,
so you will often want to update them in `EVT_CALENDAR_PAGE_CHANGED` event handler.

If neither the `wxCAL_SUNDAY_FIRST` or `wxCAL_MONDAY_FIRST` style is given, the first day
of the week is determined from operating system's settings, if possible. The native wxGTK
calendar chooses the first weekday based on locale, and these styles have no effect on it.

## Styles

This class supports the following styles:

* wxCAL_SUNDAY_FIRST: Show Sunday as the first day in the week (not in wxGTK)

* wxCAL_MONDAY_FIRST: Show Monday as the first day in the week (not in wxGTK)

* wxCAL_SHOW_HOLIDAYS: Highlight holidays in the calendar (only generic)

* wxCAL_NO_YEAR_CHANGE: Disable the year changing (deprecated, only generic)

* wxCAL_NO_MONTH_CHANGE: Disable the month (and, implicitly, the year) changing

* wxCAL_SHOW_SURROUNDING_WEEKS: Show the neighbouring weeks in the previous and next months
(only generic, always on for the native controls)

* wxCAL_SEQUENTIAL_MONTH_SELECTION: Use alternative, more compact, style for the month and
year selection controls. (only generic)

* wxCAL_SHOW_WEEK_NUMBERS: Show week numbers on the left side of the calendar. (not in
generic)

See:
* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_calendar)

* `m:wxCalendarDateAttr`

* `m:wxCalendarEvent`

* `m:wxDatePickerCtrl`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxCalendarCtrl](https://docs.wxwidgets.org/3.2/classwx_calendar_ctrl.html)

## Events

Event types emitted from this class:

* [`calendar_sel_changed`](`m:wxCalendarEvent`)

* [`calendar_weekday_clicked`](`m:wxCalendarEvent`)

# `wxCalendarCtrl`

```erlang
-type wxCalendarCtrl() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id) -> boolean()
                when This :: wxCalendarCtrl(), Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `create`

```erlang
-spec create(This, Parent, Id, [Option]) -> boolean()
                when
                    This :: wxCalendarCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Option ::
                        {date, wx:wx_datetime()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()}.
```

Creates the control.

See `wxWindow:new/3` for the meaning of the parameters and the control overview for the possible styles.

# `destroy`

```erlang
-spec destroy(This :: wxCalendarCtrl()) -> ok.
```

Destroys the object

# `enableHolidayDisplay`

```erlang
-spec enableHolidayDisplay(This) -> ok when This :: wxCalendarCtrl().
```

# `enableHolidayDisplay`

```erlang
-spec enableHolidayDisplay(This, [Option]) -> ok
                              when This :: wxCalendarCtrl(), Option :: {display, boolean()}.
```

This function should be used instead of changing `wxCAL\_SHOW\_HOLIDAYS` style bit
directly.

It enables or disables the special highlighting of the holidays.

# `enableMonthChange`

```erlang
-spec enableMonthChange(This) -> boolean() when This :: wxCalendarCtrl().
```

# `enableMonthChange`

```erlang
-spec enableMonthChange(This, [Option]) -> boolean()
                           when This :: wxCalendarCtrl(), Option :: {enable, boolean()}.
```

This function should be used instead of changing `wxCAL\_NO\_MONTH\_CHANGE` style bit.

It allows or disallows the user to change the month interactively. Note that if the month
cannot be changed, the year cannot be changed neither.

Return: true if the value of this option really changed or false if it was already set to
the requested value.

# `enableYearChange`

> This function is deprecated. wxCalendarCtrl:enableYearChange/1 is deprecated; not available in wxWidgets-2.9 and later.

```erlang
-spec enableYearChange(This) -> ok when This :: wxCalendarCtrl().
```

# `enableYearChange`

> This function is deprecated. wxCalendarCtrl:enableYearChange/2 is deprecated; not available in wxWidgets-2.9 and later.

```erlang
-spec enableYearChange(This, [Option]) -> ok
                          when This :: wxCalendarCtrl(), Option :: {enable, boolean()}.
```

Deprecated:

This function should be used instead of changing `wxCAL_NO_YEAR_CHANGE` style bit
directly. It allows or disallows the user to change the year interactively. Only in
generic `m:wxCalendarCtrl`.

# `getAttr`

```erlang
-spec getAttr(This, Day) -> wxCalendarDateAttr:wxCalendarDateAttr()
                 when This :: wxCalendarCtrl(), Day :: integer().
```

Returns the attribute for the given date (should be in the range 1...31).

The returned pointer may be NULL. Only in generic `m:wxCalendarCtrl`.

# `getDate`

```erlang
-spec getDate(This) -> wx:wx_datetime() when This :: wxCalendarCtrl().
```

Gets the currently selected date.

# `getHeaderColourBg`

```erlang
-spec getHeaderColourBg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Gets the background colour of the header part of the calendar window.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always
returns `wxNullColour` in the native versions.

See: `setHeaderColours/3`

# `getHeaderColourFg`

```erlang
-spec getHeaderColourFg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Gets the foreground colour of the header part of the calendar window.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always
returns `wxNullColour` in the native versions.

See: `setHeaderColours/3`

# `getHighlightColourBg`

```erlang
-spec getHighlightColourBg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Gets the background highlight colour.

Only in generic `m:wxCalendarCtrl`.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always
returns `wxNullColour` in the native versions.

See: `setHighlightColours/3`

# `getHighlightColourFg`

```erlang
-spec getHighlightColourFg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Gets the foreground highlight colour.

Only in generic `m:wxCalendarCtrl`.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always
returns `wxNullColour` in the native versions.

See: `setHighlightColours/3`

# `getHolidayColourBg`

```erlang
-spec getHolidayColourBg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Return the background colour currently used for holiday highlighting.

Only useful with generic `m:wxCalendarCtrl` as native versions currently don't support
holidays display at all and always return `wxNullColour`.

See: `setHolidayColours/3`

# `getHolidayColourFg`

```erlang
-spec getHolidayColourFg(This) -> wx:wx_colour4() when This :: wxCalendarCtrl().
```

Return the foreground colour currently used for holiday highlighting.

Only useful with generic `m:wxCalendarCtrl` as native versions currently don't support
holidays display at all and always return `wxNullColour`.

See: `setHolidayColours/3`

# `hitTest`

```erlang
-spec hitTest(This, Pos) -> Result
                 when
                     Result :: {Res :: wx:wx_enum(), Date :: wx:wx_datetime(), Wd :: wx:wx_enum()},
                     This :: wxCalendarCtrl(),
                     Pos :: {X :: integer(), Y :: integer()}.
```

Returns one of wxCalendarHitTestResult constants and fills either `date` or `wd` pointer
with the corresponding value depending on the hit test code.

Not implemented in wxGTK currently.

# `new`

```erlang
-spec new() -> wxCalendarCtrl().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxCalendarCtrl() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxCalendarCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {date, wx:wx_datetime()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()}.
```

Does the same as `create/4` method.

# `resetAttr`

```erlang
-spec resetAttr(This, Day) -> ok when This :: wxCalendarCtrl(), Day :: integer().
```

Clears any attributes associated with the given day (in the range 1...31).

Only in generic `m:wxCalendarCtrl`.

# `setAttr`

```erlang
-spec setAttr(This, Day, Attr) -> ok
                 when
                     This :: wxCalendarCtrl(),
                     Day :: integer(),
                     Attr :: wxCalendarDateAttr:wxCalendarDateAttr().
```

Associates the attribute with the specified date (in the range 1...31).

If the pointer is NULL, the items attribute is cleared. Only in generic `m:wxCalendarCtrl`.

# `setDate`

```erlang
-spec setDate(This, Date) -> boolean() when This :: wxCalendarCtrl(), Date :: wx:wx_datetime().
```

Sets the current date.

The `date` parameter must be valid and in the currently valid range as set by `SetDateRange()`
(not implemented in wx), otherwise the current date is not changed and the function
returns false and, additionally, triggers an assertion failure if the date is invalid.

# `setHeaderColours`

```erlang
-spec setHeaderColours(This, ColFg, ColBg) -> ok
                          when
                              This :: wxCalendarCtrl(), ColFg :: wx:wx_colour(), ColBg :: wx:wx_colour().
```

Set the colours used for painting the weekdays at the top of the control.

This method is currently only implemented in generic `m:wxCalendarCtrl` and does nothing
in the native versions.

# `setHighlightColours`

```erlang
-spec setHighlightColours(This, ColFg, ColBg) -> ok
                             when
                                 This :: wxCalendarCtrl(),
                                 ColFg :: wx:wx_colour(),
                                 ColBg :: wx:wx_colour().
```

Set the colours to be used for highlighting the currently selected date.

This method is currently only implemented in generic `m:wxCalendarCtrl` and does nothing
in the native versions.

# `setHoliday`

```erlang
-spec setHoliday(This, Day) -> ok when This :: wxCalendarCtrl(), Day :: integer().
```

Marks the specified day as being a holiday in the current month.

This method is only implemented in the generic version of the control and does nothing in
the native ones.

# `setHolidayColours`

```erlang
-spec setHolidayColours(This, ColFg, ColBg) -> ok
                           when
                               This :: wxCalendarCtrl(),
                               ColFg :: wx:wx_colour(),
                               ColBg :: wx:wx_colour().
```

Sets the colours to be used for the holidays highlighting.

This method is only implemented in the generic version of the control and does nothing in
the native ones. It should also only be called if the window style includes `wxCAL_SHOW_HOLIDAYS`
flag or `enableHolidayDisplay/2` had been called.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
