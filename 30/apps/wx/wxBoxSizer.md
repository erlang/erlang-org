# `wxBoxSizer`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxBoxSizer.erl#L58)

The basic idea behind a box sizer is that windows will most often be laid out in rather
simple basic geometry, typically in a row or a column or several hierarchies of either.

For more information, please see overview_sizer_box.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxSizer`

wxWidgets docs: [wxBoxSizer](https://docs.wxwidgets.org/3.2/classwx_box_sizer.html)

# `wxBoxSizer`

```erlang
-type wxBoxSizer() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxBoxSizer()) -> ok.
```

Destroys the object

# `getOrientation`

```erlang
-spec getOrientation(This) -> integer() when This :: wxBoxSizer().
```

Returns the orientation of the box sizer, either wxVERTICAL or wxHORIZONTAL.

# `new`

```erlang
-spec new(Orient) -> wxBoxSizer() when Orient :: integer().
```

Constructor for a `m:wxBoxSizer`.

`orient` may be either of wxVERTICAL or wxHORIZONTAL for creating either a column sizer
or a row sizer.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
