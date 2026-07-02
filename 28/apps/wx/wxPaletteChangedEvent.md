# `wxPaletteChangedEvent`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPaletteChangedEvent.erl#L58)

Functions for wxPaletteChangedEvent class

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxPaletteChangedEvent](https://docs.wxwidgets.org/3.2/classwx_palette_changed_event.html)

# `wxPaletteChanged`

```elixir
-type wxPaletteChanged() :: #wxPaletteChanged{type :: wxPaletteChangedEvent:wxPaletteChangedEventType()}.
```

# `wxPaletteChangedEvent`

```elixir
-type wxPaletteChangedEvent() :: wx:wx_object().
```

# `wxPaletteChangedEventType`

```elixir
-type wxPaletteChangedEventType() :: palette_changed.
```

# `getChangedWindow`

```elixir
-spec getChangedWindow(This) -> wxWindow:wxWindow() when This :: wxPaletteChangedEvent().
```

# `setChangedWindow`

```elixir
-spec setChangedWindow(This, Win) -> ok when This :: wxPaletteChangedEvent(), Win :: wxWindow:wxWindow().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
