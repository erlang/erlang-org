# `wxPaletteChangedEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPaletteChangedEvent.erl#L58)

Functions for wxPaletteChangedEvent class

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxPaletteChangedEvent](https://docs.wxwidgets.org/3.2/classwx_palette_changed_event.html)

# `wxPaletteChanged`

```erlang
-type wxPaletteChanged() :: #wxPaletteChanged{type :: wxPaletteChangedEvent:wxPaletteChangedEventType()}.
```

# `wxPaletteChangedEvent`

```erlang
-type wxPaletteChangedEvent() :: wx:wx_object().
```

# `wxPaletteChangedEventType`

```erlang
-type wxPaletteChangedEventType() :: palette_changed.
```

# `getChangedWindow`

```erlang
-spec getChangedWindow(This) -> wxWindow:wxWindow() when This :: wxPaletteChangedEvent().
```

# `setChangedWindow`

```erlang
-spec setChangedWindow(This, Win) -> ok when This :: wxPaletteChangedEvent(), Win :: wxWindow:wxWindow().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
