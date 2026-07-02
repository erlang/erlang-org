# `wxCalendarDateAttr`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxCalendarDateAttr.erl#L58)

`m:wxCalendarDateAttr` is a custom attributes for a calendar date.

The objects of this class are used with `m:wxCalendarCtrl`.

See: `m:wxCalendarCtrl`

wxWidgets docs: [wxCalendarDateAttr](https://docs.wxwidgets.org/3.2/classwx_calendar_date_attr.html)

# `wxCalendarDateAttr`

```elixir
-type wxCalendarDateAttr() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxCalendarDateAttr()) -> ok.
```

Destroys the object

# `getBackgroundColour`

```elixir
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the background colour set for the calendar date.

# `getBorder`

```elixir
-spec getBorder(This) -> wx:wx_enum() when This :: wxCalendarDateAttr().
```

Returns the border set for the calendar date.

# `getBorderColour`

```elixir
-spec getBorderColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the border colour set for the calendar date.

# `getFont`

```elixir
-spec getFont(This) -> wxFont:wxFont() when This :: wxCalendarDateAttr().
```

Returns the font set for the calendar date.

# `getTextColour`

```elixir
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxCalendarDateAttr().
```

Returns the text colour set for the calendar date.

# `hasBackgroundColour`

```elixir
-spec hasBackgroundColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default text background colour is set.

# `hasBorder`

```elixir
-spec hasBorder(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default (i.e. any) border is set.

# `hasBorderColour`

```elixir
-spec hasBorderColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default border colour is set.

# `hasFont`

```elixir
-spec hasFont(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default font is set.

# `hasTextColour`

```elixir
-spec hasTextColour(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if a non-default text foreground colour is set.

# `isHoliday`

```elixir
-spec isHoliday(This) -> boolean() when This :: wxCalendarDateAttr().
```

Returns true if this calendar day is displayed as a holiday.

# `new`

```elixir
-spec new() -> wxCalendarDateAttr().
```

# `new`

```elixir
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

```elixir
-spec new(Border, [Option]) -> wxCalendarDateAttr()
             when Border :: wx:wx_enum(), Option :: {colBorder, wx:wx_colour()}.
```

Constructor using default properties except the given border.

# `setBackgroundColour`

```elixir
-spec setBackgroundColour(This, ColBack) -> ok
                             when This :: wxCalendarDateAttr(), ColBack :: wx:wx_colour().
```

Sets the text background colour to use.

# `setBorder`

```elixir
-spec setBorder(This, Border) -> ok when This :: wxCalendarDateAttr(), Border :: wx:wx_enum().
```

Sets the border to use.

# `setBorderColour`

```elixir
-spec setBorderColour(This, Col) -> ok when This :: wxCalendarDateAttr(), Col :: wx:wx_colour().
```

Sets the border colour to use.

# `setFont`

```elixir
-spec setFont(This, Font) -> ok when This :: wxCalendarDateAttr(), Font :: wxFont:wxFont().
```

Sets the font to use.

# `setHoliday`

```elixir
-spec setHoliday(This, Holiday) -> ok when This :: wxCalendarDateAttr(), Holiday :: boolean().
```

If `holiday` is true, this calendar day will be displayed as a holiday.

# `setTextColour`

```elixir
-spec setTextColour(This, ColText) -> ok when This :: wxCalendarDateAttr(), ColText :: wx:wx_colour().
```

Sets the text (foreground) colour to use.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
