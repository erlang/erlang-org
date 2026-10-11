# `wxOverlay`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxOverlay.erl#L58)

Creates an overlay over an existing window, allowing for manipulations like
rubberbanding, etc.

On wxOSX the overlay is implemented with native platform APIs, on the other platforms it
is simulated using `m:wxMemoryDC`.

See:
* `m:wxDCOverlay`

* `m:wxDC`

wxWidgets docs: [wxOverlay](https://docs.wxwidgets.org/3.2/classwx_overlay.html)

# `wxOverlay`

```erlang
-type wxOverlay() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxOverlay()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxOverlay().
```

# `reset`

```erlang
-spec reset(This) -> ok when This :: wxOverlay().
```

Clears the overlay without restoring the former state.

To be done, for example, when the window content has been changed and repainted.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
