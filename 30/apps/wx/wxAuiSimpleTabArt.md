# `wxAuiSimpleTabArt`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxAuiSimpleTabArt.erl#L58)

Another standard tab art provider for `m:wxAuiNotebook`.

`m:wxAuiSimpleTabArt` is derived from `m:wxAuiTabArt` demonstrating how to write a
completely new tab art class. It can also be used as alternative to `wxAuiDefaultTabArt`
(not implemented in wx).

This class is derived, and can use functions, from:

* `m:wxAuiTabArt`

wxWidgets docs: [wxAuiSimpleTabArt](https://docs.wxwidgets.org/3.2/classwx_aui_simple_tab_art.html)

# `wxAuiSimpleTabArt`

```erlang
-type wxAuiSimpleTabArt() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxAuiSimpleTabArt()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxAuiSimpleTabArt().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
