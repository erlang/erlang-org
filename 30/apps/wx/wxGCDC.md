# `wxGCDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxGCDC.erl#L58)

`m:wxGCDC` is a device context that draws on a `m:wxGraphicsContext`.

`m:wxGCDC` does its best to implement `m:wxDC` API, but the following features are not
(fully) implemented because `m:wxGraphicsContext` doesn't support them:

* `wxDC:getPixel/2` method is not implemented and always returns false because modern graphics layers don't
support retrieving the contents of the drawn pixels.

* `wxDC:floodFill/4` method is not, and can't be, implemented, as its functionality relies on reading the
pixels from `m:wxGraphicsContext` too.

* `wxDC:setLogicalFunction/2` method only works with `wxCOPY`, `wxOR`, `wxNO_OP`, `wxCLEAR` and `wxXOR` functions,
attempts to use any other function (including `wxINVERT`) don't do anything.

* Similarly, ?wxRasterOperationMode parameter of `wxDC:blit/6` and `StretchBlit()` (not implemented in
wx) can only be one of the supported logical functions listed above, using any other
function will result in an assertion failure and not drawing anything.

* For Direct2D-based `m:wxGraphicsContext`, only true-type fonts can be used in the
font-related functions.

See:
* `m:wxDC`

* `m:wxGraphicsContext`

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxGCDC](https://docs.wxwidgets.org/3.2/classwx_g_c_d_c.html)

# `wxGCDC`

```erlang
-type wxGCDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxGCDC()) -> ok.
```

Destroys the object

# `getGraphicsContext`

```erlang
-spec getGraphicsContext(This) -> wxGraphicsContext:wxGraphicsContext() when This :: wxGCDC().
```

Retrieves associated `m:wxGraphicsContext`.

# `new`

```erlang
-spec new() -> wxGCDC().
```

# `new`

```erlang
-spec new(WindowDC) -> wxGCDC()
             when
                 WindowDC ::
                     wxWindowDC:wxWindowDC() |
                     wxMemoryDC:wxMemoryDC() |
                     wxGraphicsContext:wxGraphicsContext().
```

Constructs a `m:wxGCDC` from a `m:wxWindowDC`.

# `setGraphicsContext`

```erlang
-spec setGraphicsContext(This, Context) -> ok
                            when This :: wxGCDC(), Context :: wxGraphicsContext:wxGraphicsContext().
```

Set the graphics context to be used for this `m:wxGCDC`.

Note that this object takes ownership of `context` and will delete it when it is
destroyed or when `setGraphicsContext/2` is called again.

Also, unlike the constructor taking `m:wxGraphicsContext`, this method will reapply the
current font, pen and brush, so that this object continues to use them, if they had been
changed before (which is never the case when constructing `m:wxGCDC` directly from `m:wxGraphicsContext`).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
