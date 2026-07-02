# `wxAuiTabArt`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxAuiTabArt.erl#L58)

Tab art provider defines all the drawing functions used by `m:wxAuiNotebook`.

This allows the `m:wxAuiNotebook` to have a pluggable look-and-feel.

By default, a `m:wxAuiNotebook` uses an instance of this class called `wxAuiDefaultTabArt`
(not implemented in wx) which provides bitmap art and a colour scheme that is adapted to
the major platforms' look. You can either derive from that class to alter its behaviour or
write a completely new tab art class.

Another example of creating a new `m:wxAuiNotebook` tab bar is `m:wxAuiSimpleTabArt`.

Call `wxAuiNotebook:setArtProvider/2` to make use of this new tab art.

wxWidgets docs: [wxAuiTabArt](https://docs.wxwidgets.org/3.2/classwx_aui_tab_art.html)

# `wxAuiTabArt`

```elixir
-type wxAuiTabArt() :: wx:wx_object().
```

# `setActiveColour`

```elixir
-spec setActiveColour(This, Colour) -> ok when This :: wxAuiTabArt(), Colour :: wx:wx_colour().
```

Sets the colour of the selected tab.

Since: 2.9.2

# `setColour`

```elixir
-spec setColour(This, Colour) -> ok when This :: wxAuiTabArt(), Colour :: wx:wx_colour().
```

Sets the colour of the inactive tabs.

Since: 2.9.2

# `setFlags`

```elixir
-spec setFlags(This, Flags) -> ok when This :: wxAuiTabArt(), Flags :: integer().
```

Sets flags.

# `setMeasuringFont`

```elixir
-spec setMeasuringFont(This, Font) -> ok when This :: wxAuiTabArt(), Font :: wxFont:wxFont().
```

Sets the font used for calculating measurements.

# `setNormalFont`

```elixir
-spec setNormalFont(This, Font) -> ok when This :: wxAuiTabArt(), Font :: wxFont:wxFont().
```

Sets the normal font for drawing labels.

# `setSelectedFont`

```elixir
-spec setSelectedFont(This, Font) -> ok when This :: wxAuiTabArt(), Font :: wxFont:wxFont().
```

Sets the font for drawing text for selected UI elements.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
