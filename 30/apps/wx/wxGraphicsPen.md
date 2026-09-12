# `wxGraphicsPen`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsPen.erl#L58)

A `m:wxGraphicsPen` is a native representation of a pen.

The contents are specific and private to the respective renderer. Instances are ref
counted and can therefore be assigned as usual. The only way to get a valid instance is
via `wxGraphicsContext:createPen/2` or `wxGraphicsRenderer::CreatePen()` (not implemented in wx).

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsPen](https://docs.wxwidgets.org/3.2/classwx_graphics_pen.html)

# `wxGraphicsPen`

```erlang
-type wxGraphicsPen() :: wx:wx_object().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
