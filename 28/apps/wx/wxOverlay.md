# `wxOverlay`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxOverlay.erl#L58)

Creates an overlay over an existing window, allowing for manipulations like
rubberbanding, etc.

On wxOSX the overlay is implemented with native platform APIs, on the other platforms it
is simulated using `m:wxMemoryDC`.

See:
* `m:wxDCOverlay`

* `m:wxDC`

wxWidgets docs: [wxOverlay](https://docs.wxwidgets.org/3.2/classwx_overlay.html)

# `wxOverlay`

```elixir
-type wxOverlay() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxOverlay()) -> ok.
```

Destroys the object

# `new`

```elixir
-spec new() -> wxOverlay().
```

# `reset`

```elixir
-spec reset(This) -> ok when This :: wxOverlay().
```

Clears the overlay without restoring the former state.

To be done, for example, when the window content has been changed and repainted.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
