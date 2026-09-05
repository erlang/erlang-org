# `wxMirrorDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxMirrorDC.erl#L58)

`m:wxMirrorDC` is a simple wrapper class which is always associated with a real `m:wxDC`
object and either forwards all of its operations to it without changes (no mirroring takes
place) or exchanges `x` and `y` coordinates which makes it possible to reuse the same code
to draw a figure and its mirror -- i.e.

reflection related to the diagonal line x == y.

Since: 2.5.0

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxMirrorDC](https://docs.wxwidgets.org/3.2/classwx_mirror_d_c.html)

# `wxMirrorDC`

```erlang
-type wxMirrorDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxMirrorDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new(Dc, Mirror) -> wxMirrorDC() when Dc :: wxDC:wxDC(), Mirror :: boolean().
```

Creates a (maybe) mirrored DC associated with the real `dc`.

Everything drawn on `m:wxMirrorDC` will appear (and maybe mirrored) on `dc`.

`mirror` specifies if we do mirror (if it is true) or not (if it is false).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
