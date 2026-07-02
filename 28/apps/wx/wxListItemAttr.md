# `wxListItemAttr`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxListItemAttr.erl#L58)

Functions for wxListItemAttr class

wxWidgets docs: [wxListItemAttr](https://docs.wxwidgets.org/3.2/classwx_list_item_attr.html)

# `wxListItemAttr`

```elixir
-type wxListItemAttr() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxListItemAttr()) -> ok.
```

Destroys the object

# `getBackgroundColour`

```elixir
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxListItemAttr().
```

# `getFont`

```elixir
-spec getFont(This) -> wxFont:wxFont() when This :: wxListItemAttr().
```

# `getTextColour`

```elixir
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxListItemAttr().
```

# `hasBackgroundColour`

```elixir
-spec hasBackgroundColour(This) -> boolean() when This :: wxListItemAttr().
```

# `hasFont`

```elixir
-spec hasFont(This) -> boolean() when This :: wxListItemAttr().
```

# `hasTextColour`

```elixir
-spec hasTextColour(This) -> boolean() when This :: wxListItemAttr().
```

# `new`

```elixir
-spec new() -> wxListItemAttr().
```

# `new`

```elixir
-spec new(ColText, ColBack, Font) -> wxListItemAttr()
             when ColText :: wx:wx_colour(), ColBack :: wx:wx_colour(), Font :: wxFont:wxFont().
```

# `setBackgroundColour`

```elixir
-spec setBackgroundColour(This, ColBack) -> ok when This :: wxListItemAttr(), ColBack :: wx:wx_colour().
```

# `setFont`

```elixir
-spec setFont(This, Font) -> ok when This :: wxListItemAttr(), Font :: wxFont:wxFont().
```

# `setTextColour`

```elixir
-spec setTextColour(This, ColText) -> ok when This :: wxListItemAttr(), ColText :: wx:wx_colour().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
