# `wxCalendarDateAttr`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxCalendarDateAttr.erl#L58)

`m:wxCalendarDateAttr` is a custom attributes for a calendar date.

The objects of this class are used with `m:wxCalendarCtrl`.

See: `m:wxCalendarCtrl`

wxWidgets docs: [wxCalendarDateAttr](https://docs.wxwidgets.org/3.2/classwx_calendar_date_attr.html)

# `wxCalendarDateAttr`

```erlang
-type wxCalendarDateAttr() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxCalendarDateAttr()) -> ok.
```

Destroys the object

# `getBackgroundColour`

```erlang
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the background colour set for the calendar date.

# `getBorder`

```erlang
-spec getBorder(This) -> wx:wx_enum() when This :: wxCalendarDateAttr().
```

Returns the border set for the calendar date.

# `getBorderColour`

```erlang
-spec getBorderColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the border colour set for the calendar date.

# `getFont`

```erlang
-spec getFont(This) -> wxFont:wxFont() when This :: wxCalendarDateAttr().
```

Returns the font set for the calendar date.

# `getTextColour`

```erlang
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the text colour set for the calendar date.

# `hasBackgroundColour`

```erlang
-spec hasBackgroundColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default text background colour is set.

# `hasBorder`

```erlang
-spec hasBorder(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default (i.e. any) border is set.

# `hasBorderColour`

```erlang
-spec hasBorderColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default border colour is set.

# `hasFont`

```erlang
-spec hasFont(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default font is set.

# `hasTextColour`

```erlang
-spec hasTextColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default text foreground colour is set.

# `isHoliday`

```erlang
-spec isHoliday(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if this calendar day is displayed as a holiday.

# `new`

```erlang
-spec new() -> wxCalendarDateAttr().
```

# `new`

```erlang
-spec new(Border) -> wxCalendarDateAttr() when Border :: wx:wx_enum();
         ([Option]) -> wxCalendarDateAttr()
             when
                 Option ::
                     {colText, wx:wx_colour()} |
                     {colBack, wx:wx_colour()} |
                     {colBorder, wx:wx_colour()} |
                     {font, wxFont:wxFont()} |
                     {border, wx:wx_enum()}.
```

Constructor for specifying all `m:wxCalendarDateAttr` properties.

# `new`

```erlang
-spec new(Border, [Option]) -> wxCalendarDateAttr()
             when Border :: wx:wx_enum(), Option :: {colBorder, wx:wx_colour()}.
```

Constructor using default properties except the given border.

# `setBackgroundColour`

```erlang
-spec setBackgroundColour(This, ColBack) -> ok
                             when This :: wxCalendarDateAttr(), ColBack :: wx:wx_colour().
```

Sets the text background colour to use.

# `setBorder`

```erlang
-spec setBorder(This, Border) -> ok when This :: wxCalendarDateAttr(), Border :: wx:wx_enum().
```

Sets the border to use.

# `setBorderColour`

```erlang
-spec setBorderColour(This, Col) -> ok when This :: wxCalendarDateAttr(), Col :: wx:wx_colour().
```

Sets the border colour to use.

# `setFont`

```erlang
-spec setFont(This, Font) -> ok when This :: wxCalendarDateAttr(), Font :: wxFont:wxFont().
```

Sets the font to use.

# `setHoliday`

```erlang
-spec setHoliday(This, Holiday) -> ok when This :: wxCalendarDateAttr(), Holiday :: boolean().
```

If `holiday` is true, this calendar day will be displayed as a holiday.

# `setTextColour`

```erlang
-spec setTextColour(This, ColText) -> ok when This :: wxCalendarDateAttr(), ColText :: wx:wx_colour().
```

Sets the text (foreground) colour to use.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
