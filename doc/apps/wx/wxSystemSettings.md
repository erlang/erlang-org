# `wxSystemSettings`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxSystemSettings.erl#L58)

`m:wxSystemSettings` allows the application to ask for details about the system.

This can include settings such as standard colours, fonts, and user interface element sizes.

See:
* `m:wxFont`

* `wx_color()`

* `m:wxSystemOptions`

wxWidgets docs: [wxSystemSettings](https://docs.wxwidgets.org/3.2/classwx_system_settings.html)

# `wxSystemSettings`

```erlang
-type wxSystemSettings() :: wx:wx_object().
```

# `getColour`

```erlang
-spec getColour(Index) -> wx:wx_colour4() when Index :: wx:wx_enum().
```

Returns a system colour.

Return: The returned colour is always valid.

# `getFont`

```erlang
-spec getFont(Index) -> wxFont:wxFont() when Index :: wx:wx_enum().
```

Returns a system font.

Return: The returned font is always valid.

# `getMetric`

```erlang
-spec getMetric(Index) -> integer() when Index :: wx:wx_enum().
```

# `getMetric`

```erlang
-spec getMetric(Index, [Option]) -> integer()
                   when Index :: wx:wx_enum(), Option :: {win, wxWindow:wxWindow()}.
```

Returns the value of a system metric, or -1 if the metric is not supported on the current
system.

The value of `win` determines if the metric returned is a global value or a `m:wxWindow`
based value, in which case it might determine the widget, the display the window is on, or
something similar. The window given should be as close to the metric as possible (e.g. a `m:wxTopLevelWindow`
in case of the wxSYS_CAPTION_Y metric).

`index` can be one of the ?wxSystemMetric enum values.

`win` is a pointer to the window for which the metric is requested. Specifying the `win`
parameter is encouraged, because some metrics on some ports are not supported without
one,or they might be capable of reporting better values if given one. If a window does not
make sense for a metric, one should still be given, as for example it might determine
which displays cursor width is requested with wxSYS_CURSOR_X.

# `getScreenType`

```erlang
-spec getScreenType() -> wx:wx_enum().
```

Returns the screen type.

The return value is one of the ?wxSystemScreenType enum values.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
