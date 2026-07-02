# `wxQueryNewPaletteEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxQueryNewPaletteEvent.erl#L58)

Functions for wxQueryNewPaletteEvent class

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxQueryNewPaletteEvent](https://docs.wxwidgets.org/3.2/classwx_query_new_palette_event.html)

# `wxQueryNewPalette`

```elixir
-type wxQueryNewPalette() ::
          #wxQueryNewPalette{type :: wxQueryNewPaletteEvent:wxQueryNewPaletteEventType()}.
```

# `wxQueryNewPaletteEvent`

```elixir
-type wxQueryNewPaletteEvent() :: wx:wx_object().
```

# `wxQueryNewPaletteEventType`

```elixir
-type wxQueryNewPaletteEventType() :: query_new_palette.
```

# `getPaletteRealized`

```elixir
-spec getPaletteRealized(This) -> boolean() when This :: wxQueryNewPaletteEvent().
```

# `setPaletteRealized`

```elixir
-spec setPaletteRealized(This, Realized) -> ok
                            when This :: wxQueryNewPaletteEvent(), Realized :: boolean().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
