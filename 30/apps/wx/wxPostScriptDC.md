# `wxPostScriptDC`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxPostScriptDC.erl#L58)

This defines the wxWidgets Encapsulated PostScript device context, which can write
PostScript files on any platform.

See `m:wxDC` for descriptions of the member functions.

Starting a document

Document should be started with call to `wxDC:startDoc/2` prior to calling any function to execute a
drawing operation. However, some functions, like `wxDC:setFont/2`, may be legitimately called even before `wxDC:startDoc/2`.

This class is derived, and can use functions, from:

* `m:wxDC`

wxWidgets docs: [wxPostScriptDC](https://docs.wxwidgets.org/3.2/classwx_post_script_d_c.html)

# `wxPostScriptDC`

```erlang
-type wxPostScriptDC() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxPostScriptDC()) -> ok.
```

Destroys the object

# `new`

```erlang
-spec new() -> wxPostScriptDC().
```

# `new`

```erlang
-spec new(PrintData) -> wxPostScriptDC() when PrintData :: wxPrintData:wxPrintData().
```

Constructs a PostScript printer device context from a `m:wxPrintData` object.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
