# `wxGraphicsFont`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsFont.erl#L58)

A `m:wxGraphicsFont` is a native representation of a font.

The contents are specific and private to the respective renderer. Instances are ref
counted and can therefore be assigned as usual. The only way to get a valid instance is
via `wxGraphicsContext:createFont/4` or `wxGraphicsRenderer:createFont/4`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsFont](https://docs.wxwidgets.org/3.2/classwx_graphics_font.html)

# `wxGraphicsFont`

```erlang
-type wxGraphicsFont() :: wx:wx_object().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
