# `wxListItemAttr`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxListItemAttr.erl#L58)

Functions for wxListItemAttr class

wxWidgets docs: [wxListItemAttr](https://docs.wxwidgets.org/3.2/classwx_list_item_attr.html)

# `wxListItemAttr`

```erlang
-type wxListItemAttr() :: wx:wx_object().
```

# `destroy`

```erlang
-spec destroy(This :: wxListItemAttr()) -> ok.
```

Destroys the object

# `getBackgroundColour`

```erlang
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxListItemAttr().
```

# `getFont`

```erlang
-spec getFont(This) -> wxFont:wxFont() when This :: wxListItemAttr().
```

# `getTextColour`

```erlang
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxListItemAttr().
```

# `hasBackgroundColour`

```erlang
-spec hasBackgroundColour(This) -> boolean() when This :: wxListItemAttr().
```

# `hasFont`

```erlang
-spec hasFont(This) -> boolean() when This :: wxListItemAttr().
```

# `hasTextColour`

```erlang
-spec hasTextColour(This) -> boolean() when This :: wxListItemAttr().
```

# `new`

```erlang
-spec new() -> wxListItemAttr().
```

# `new`

```erlang
-spec new(ColText, ColBack, Font) -> wxListItemAttr()
             when ColText :: wx:wx_colour(), ColBack :: wx:wx_colour(), Font :: wxFont:wxFont().
```

# `setBackgroundColour`

```erlang
-spec setBackgroundColour(This, ColBack) -> ok when This :: wxListItemAttr(), ColBack :: wx:wx_colour().
```

# `setFont`

```erlang
-spec setFont(This, Font) -> ok when This :: wxListItemAttr(), Font :: wxFont:wxFont().
```

# `setTextColour`

```erlang
-spec setTextColour(This, ColText) -> ok when This :: wxListItemAttr(), ColText :: wx:wx_colour().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
