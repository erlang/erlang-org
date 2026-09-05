# `wxGraphicsObject`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGraphicsObject.erl#L58)

This class is the superclass of native graphics objects like pens etc.

It allows reference counting. Not instantiated by user code.

See:
* `m:wxGraphicsBrush`

* `m:wxGraphicsPen`

* `m:wxGraphicsMatrix`

* `m:wxGraphicsPath`

wxWidgets docs: [wxGraphicsObject](https://docs.wxwidgets.org/3.2/classwx_graphics_object.html)

# `wxGraphicsObject`

```erlang
-type wxGraphicsObject() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGraphicsObject()) -> ok.
```

Destroys the object

# `getRenderer`

```erlang
-spec getRenderer(This) -> wxGraphicsRenderer:wxGraphicsRenderer() when This :: wxGraphicsObject().
```

Returns the renderer that was used to create this instance, or NULL if it has not been
initialized yet.

# `isNull`

```erlang
-spec isNull(This) -> boolean() when This :: wxGraphicsObject().
```

Return: false if this object is valid, otherwise returns true.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
