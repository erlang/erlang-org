# `wxColourData`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxColourData.erl#L58)

This class holds a variety of information related to colour dialogs.

See:
* `wx_color()`

* `m:wxColourDialog`

* [Overview cmndlg](https://docs.wxwidgets.org/3.2/overview_cmndlg.html#overview_cmndlg_colour)

wxWidgets docs: [wxColourData](https://docs.wxwidgets.org/3.2/classwx_colour_data.html)

# `wxColourData`

```elixir
-type wxColourData() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxColourData()) -> ok.
```

Destroys the object

# `getChooseFull`

```elixir
-spec getChooseFull(This) -> boolean() when This :: wxColourData().
```

Under Windows, determines whether the Windows colour dialog will display the full dialog
with custom colour selection controls.

Has no meaning under other platforms.

The default value is true.

# `getColour`

```elixir
-spec getColour(This) -> wx:wx_colour4() when This :: wxColourData().
```

Gets the current colour associated with the colour dialog.

The default colour is black.

# `getCustomColour`

```elixir
-spec getCustomColour(This, I) -> wx:wx_colour4() when This :: wxColourData(), I :: integer().
```

Returns custom colours associated with the colour dialog.

# `new`

```elixir
-spec new() -> wxColourData().
```

Constructor.

Initializes the custom colours to `wxNullColour`, the `data` colour setting to black, and
the `choose` full setting to true.

# `setChooseFull`

```elixir
-spec setChooseFull(This, Flag) -> ok when This :: wxColourData(), Flag :: boolean().
```

Under Windows, tells the Windows colour dialog to display the full dialog with custom
colour selection controls.

Under other platforms, has no effect.

The default value is true.

# `setColour`

```elixir
-spec setColour(This, Colour) -> ok when This :: wxColourData(), Colour :: wx:wx_colour().
```

Sets the default colour for the colour dialog.

The default colour is black.

# `setCustomColour`

```elixir
-spec setCustomColour(This, I, Colour) -> ok
                         when This :: wxColourData(), I :: integer(), Colour :: wx:wx_colour().
```

Sets custom colours for the colour dialog.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
