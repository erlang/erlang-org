# `wxGraphicsBrush`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsBrush.erl#L58)

A `m:wxGraphicsBrush` is a native representation of a brush.

The contents are specific and private to the respective renderer. Instances are ref
counted and can therefore be assigned as usual. The only way to get a valid instance is
via `wxGraphicsContext:createBrush/2` or `wxGraphicsRenderer:createBrush/2`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsBrush](https://docs.wxwidgets.org/3.2/classwx_graphics_brush.html)

# `wxGraphicsBrush`

```erlang
-type wxGraphicsBrush() :: wx:wx_object().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
