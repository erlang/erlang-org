# `wxFontData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxFontData.erl#L58)

This class holds a variety of information related to font dialogs.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_font)

* `m:wxFont`

* `m:wxFontDialog`

wxWidgets docs: [wxFontData](https://docs.wxwidgets.org/3.2/classwx_font_data.html)

# `wxFontData`

```elixir
-type wxFontData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxFontData()) -> ok.
```

Destroys the object

# `enableEffects`

```elixir
-spec enableEffects(This, Enable) -> ok when This :: wxFontData(), Enable :: boolean().
```

Enables or disables "effects" under Windows or generic only.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.

# `getAllowSymbols`

```elixir
-spec getAllowSymbols(This) -> boolean() when This :: wxFontData().
```

Under Windows, returns a flag determining whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.

# `getChosenFont`

```elixir
-spec getChosenFont(This) -> wxFont:wxFont() when This :: wxFontData().
```

Gets the font chosen by the user if the user pressed OK (`wxFontDialog::ShowModal()` (not
implemented in wx) returned wxID\_OK).

# `getColour`

```elixir
-spec getColour(This) -> wx:wx_colour4() when This :: wxFontData().
```

Gets the colour associated with the font dialog.

The default value is black.

# `getEnableEffects`

```elixir
-spec getEnableEffects(This) -> boolean() when This :: wxFontData().
```

Determines whether "effects" are enabled under Windows.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.

# `getInitialFont`

```elixir
-spec getInitialFont(This) -> wxFont:wxFont() when This :: wxFontData().
```

Gets the font that will be initially used by the font dialog.

This should have previously been set by the application.

# `getShowHelp`

```elixir
-spec getShowHelp(This) -> boolean() when This :: wxFontData().
```

Returns true if the Help button will be shown (Windows only).

The default value is false.

# `new`

```elixir
-spec new() -> wxFontData().
```

Constructor.

Initializes `fontColour` to black, `showHelp` to false, `allowSymbols` to true, `enableEffects`
to true, `minSize` to 0 and `maxSize` to 0.

# `new`

```elixir
-spec new(Data) -> wxFontData() when Data :: wxFontData().
```

Copy Constructor.

# `setAllowSymbols`

```elixir
-spec setAllowSymbols(This, AllowSymbols) -> ok when This :: wxFontData(), AllowSymbols :: boolean().
```

Under Windows, determines whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.

# `setChosenFont`

```elixir
-spec setChosenFont(This, Font) -> ok when This :: wxFontData(), Font :: wxFont:wxFont().
```

Sets the font that will be returned to the user (for internal use only).

# `setColour`

```elixir
-spec setColour(This, Colour) -> ok when This :: wxFontData(), Colour :: wx:wx_colour().
```

Sets the colour that will be used for the font foreground colour.

The default colour is black.

# `setInitialFont`

```elixir
-spec setInitialFont(This, Font) -> ok when This :: wxFontData(), Font :: wxFont:wxFont().
```

Sets the font that will be initially used by the font dialog.

# `setRange`

```elixir
-spec setRange(This, Min, Max) -> ok when This :: wxFontData(), Min :: integer(), Max :: integer().
```

Sets the valid range for the font point size (Windows only).

The default is 0, 0 (unrestricted range).

# `setShowHelp`

```elixir
-spec setShowHelp(This, ShowHelp) -> ok when This :: wxFontData(), ShowHelp :: boolean().
```

Determines whether the Help button will be displayed in the font dialog (Windows only).

The default value is false.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
