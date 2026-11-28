# `wxDCOverlay`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxDCOverlay.erl#L58)

Connects an overlay with a drawing DC.

See:
* `m:wxOverlay`

* `m:wxDC`

wxWidgets docs: [wxDCOverlay](https://docs.wxwidgets.org/3.2/classwx_d_c_overlay.html)

# `wxDCOverlay`

```erlang
-type wxDCOverlay() :: wx:wx_object().
```

# `clear`

```erlang
-spec clear(This) -> ok when This :: wxDCOverlay().
```

Clears the layer, restoring the state at the last init.

# `destroy`

```erlang
-spec destroy(This :: wxDCOverlay()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Overlay, Dc) -> wxDCOverlay() when Overlay :: wxOverlay:wxOverlay(), Dc :: wxDC:wxDC().
```

Convenience wrapper that behaves the same using the entire area of the dc.

# `new`

```erlang
-spec new(Overlay, Dc, X, Y, Width, Height) -> wxDCOverlay()
             when
                 Overlay :: wxOverlay:wxOverlay(),
                 Dc :: wxDC:wxDC(),
                 X :: integer(),
                 Y :: integer(),
                 Width :: integer(),
                 Height :: integer().
```

Connects this overlay to the corresponding drawing dc, if the overlay is not initialized
yet this call will do so.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
