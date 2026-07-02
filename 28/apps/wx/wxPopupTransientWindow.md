# `wxPopupTransientWindow`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxPopupTransientWindow.erl#L58)

A `m:wxPopupWindow` which disappears automatically when the user clicks mouse outside it
or if it loses focus in any other way.

This window can be useful for implementing custom combobox-like controls for example.

See: `m:wxPopupWindow`

This class is derived, and can use functions, from:

* `m:wxPopupWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxPopupTransientWindow](https://docs.wxwidgets.org/3.2/classwx_popup_transient_window.html)

# `wxPopupTransientWindow`

```elixir
-type wxPopupTransientWindow() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxPopupTransientWindow()) -> ok.
```

Destroys the object

# `dismiss`

```elixir
-spec dismiss(This) -> ok when This :: wxPopupTransientWindow().
```

Hide the window.

# `new`

```elixir
-spec new() -> wxPopupTransientWindow().
```

Default constructor.

# `new`

```elixir
-spec new(Parent) -> wxPopupTransientWindow() when Parent :: wxWindow:wxWindow().
```

# `new`

```elixir
-spec new(Parent, [Option]) -> wxPopupTransientWindow()
             when Parent :: wxWindow:wxWindow(), Option :: {style, integer()}.
```

Constructor.

# `popup`

```elixir
-spec popup(This) -> ok when This :: wxPopupTransientWindow().
```

# `popup`

```elixir
-spec popup(This, [Option]) -> ok
               when This :: wxPopupTransientWindow(), Option :: {focus, wxWindow:wxWindow()}.
```

Popup the window (this will show it too).

If `focus` is non-NULL, it will be kept focused while this window is shown if supported
by the current platform, otherwise the popup itself will receive focus. In any case, the
popup will disappear automatically if it loses focus because of a user action.

See: `dismiss/1`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
