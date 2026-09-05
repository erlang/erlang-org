# `wxQueryNewPaletteEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxQueryNewPaletteEvent.erl#L58)

Functions for wxQueryNewPaletteEvent class

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxQueryNewPaletteEvent](https://docs.wxwidgets.org/3.2/classwx_query_new_palette_event.html)

# `wxQueryNewPalette`

```erlang
-type wxQueryNewPalette() ::
          #wxQueryNewPalette{type :: wxQueryNewPaletteEvent:wxQueryNewPaletteEventType()}.
```

# `wxQueryNewPaletteEvent`

```erlang
-type wxQueryNewPaletteEvent() :: wx:wx_object().
```

# `wxQueryNewPaletteEventType`

```erlang
-type wxQueryNewPaletteEventType() :: query_new_palette.
```

# `getPaletteRealized`

```erlang
-spec getPaletteRealized(This) -> boolean() when This :: wxQueryNewPaletteEvent().
```

# `setPaletteRealized`

```erlang
-spec setPaletteRealized(This, Realized) -> ok
                            when This :: wxQueryNewPaletteEvent(), Realized :: boolean().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
