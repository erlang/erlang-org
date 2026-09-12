# `wxFontData`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxFontData.erl#L58)

This class holds a variety of information related to font dialogs.

See:
* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_font)

* `m:wxFont`

* `m:wxFontDialog`

wxWidgets docs: [wxFontData](https://docs.wxwidgets.org/3.2/classwx_font_data.html)

# `wxFontData`

```erlang
-type wxFontData() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxFontData()) -> ok.
```

Destroys the object

# `enableEffects`

```erlang
-spec enableEffects(This, Enable) -> ok when This :: wxFontData(), Enable :: boolean().
```

Enables or disables "effects" under Windows or generic only.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.

# `getAllowSymbols`

```erlang
-spec getAllowSymbols(This) -> boolean() when This :: wxFontData().
```

Under Windows, returns a flag determining whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.

# `getChosenFont`

```erlang
-spec getChosenFont(This) -> wxFont:wxFont() when This :: wxFontData().
```

Gets the font chosen by the user if the user pressed OK (`wxFontDialog::ShowModal()` (not
implemented in wx) returned wxID\_OK).

# `getColour`

```erlang
-spec getColour(This) -> wx:wx_colour4() when This :: wxFontData().
```

Gets the colour associated with the font dialog.

The default value is black.

# `getEnableEffects`

```erlang
-spec getEnableEffects(This) -> boolean() when This :: wxFontData().
```

Determines whether "effects" are enabled under Windows.

This refers to the controls for manipulating colour, strikeout and underline properties.

The default value is true.

# `getInitialFont`

```erlang
-spec getInitialFont(This) -> wxFont:wxFont() when This :: wxFontData().
```

Gets the font that will be initially used by the font dialog.

This should have previously been set by the application.

# `getShowHelp`

```erlang
-spec getShowHelp(This) -> boolean() when This :: wxFontData().
```

Returns true if the Help button will be shown (Windows only).

The default value is false.

# `new`

```erlang
-spec new() -> wxFontData().
```

Constructor.

Initializes `fontColour` to black, `showHelp` to false, `allowSymbols` to true, `enableEffects`
to true, `minSize` to 0 and `maxSize` to 0.

# `new`

```erlang
-spec new(Data) -> wxFontData() when Data :: wxFontData().
```

Copy Constructor.

# `setAllowSymbols`

```erlang
-spec setAllowSymbols(This, AllowSymbols) -> ok when This :: wxFontData(), AllowSymbols :: boolean().
```

Under Windows, determines whether symbol fonts can be selected.

Has no effect on other platforms.

The default value is true.

# `setChosenFont`

```erlang
-spec setChosenFont(This, Font) -> ok when This :: wxFontData(), Font :: wxFont:wxFont().
```

Sets the font that will be returned to the user (for internal use only).

# `setColour`

```erlang
-spec setColour(This, Colour) -> ok when This :: wxFontData(), Colour :: wx:wx_colour().
```

Sets the colour that will be used for the font foreground colour.

The default colour is black.

# `setInitialFont`

```erlang
-spec setInitialFont(This, Font) -> ok when This :: wxFontData(), Font :: wxFont:wxFont().
```

Sets the font that will be initially used by the font dialog.

# `setRange`

```erlang
-spec setRange(This, Min, Max) -> ok when This :: wxFontData(), Min :: integer(), Max :: integer().
```

Sets the valid range for the font point size (Windows only).

The default is 0, 0 (unrestricted range).

# `setShowHelp`

```erlang
-spec setShowHelp(This, ShowHelp) -> ok when This :: wxFontData(), ShowHelp :: boolean().
```

Determines whether the Help button will be displayed in the font dialog (Windows only).

The default value is false.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
