# `wxIconBundle`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxIconBundle.erl#L58)

This class contains multiple copies of an icon in different sizes.

It is typically used in `wxDialog::SetIcons` (not implemented in wx) and `wxTopLevelWindow:setIcons/2`.

Predefined objects (include wx.hrl): ?wxNullIconBundle

wxWidgets docs: [wxIconBundle](https://docs.wxwidgets.org/3.2/classwx_icon_bundle.html)

# `wxIconBundle`

```elixir
-type wxIconBundle() :: wx:wx_object().
```

# `addIcon`

```elixir
-spec addIcon(This, File) -> ok when This :: wxIconBundle(), File :: unicode:chardata();
             (This, Icon) -> ok when This :: wxIconBundle(), Icon :: wxIcon:wxIcon().
```

Adds the icon to the collection; if the collection already contains an icon with the same
width and height, it is replaced by the new one.

# `addIcon`

```elixir
-spec addIcon(This, File, Type) -> ok
                 when This :: wxIconBundle(), File :: unicode:chardata(), Type :: wx:wx_enum().
```

# `destroy`

```elixir
-spec destroy(This :: wxIconBundle()) -> ok.
```

Destroys the object

# `getIcon`

```elixir
-spec getIcon(This) -> wxIcon:wxIcon() when This :: wxIconBundle().
```

# `getIcon`

```elixir
-spec getIcon(This, Size) -> wxIcon:wxIcon()
                 when This :: wxIconBundle(), Size :: {W :: integer(), H :: integer()};
             (This, [Option]) -> wxIcon:wxIcon()
                 when This :: wxIconBundle(), Option :: {size, integer()} | {flags, integer()}.
```

Same as.

.

# `getIcon`

```elixir
-spec getIcon(This, Size, [Option]) -> wxIcon:wxIcon()
                 when
                     This :: wxIconBundle(),
                     Size :: {W :: integer(), H :: integer()},
                     Option :: {flags, integer()}.
```

Returns the icon with the given size.

If `size` is ?wxDefaultSize, it is interpreted as the standard system icon size, i.e. the
size returned by `wxSystemSettings:getMetric/2` for `wxSYS_ICON_X` and `wxSYS_ICON_Y`.

If the bundle contains an icon with exactly the requested size, it's always returned.
Otherwise, the behaviour depends on the flags. If only `wxIconBundle::FALLBACK_NONE` (not
implemented in wx) is given, the function returns an invalid icon. If `wxIconBundle::FALLBACK_SYSTEM`
(not implemented in wx) is given, it tries to find the icon of standard system size,
regardless of the size passed as parameter. Otherwise, or if the icon system size is not
found neither, but `wxIconBundle::FALLBACK_NEAREST_LARGER` (not implemented in wx) flag is
specified, the function returns the smallest icon of the size larger than the requested
one or, if this fails too, just the icon closest to the specified size.

The `flags` parameter is available only since wxWidgets 2.9.4.

# `new`

```elixir
-spec new() -> wxIconBundle().
```

Default ctor.

# `new`

```elixir
-spec new(Ic) -> wxIconBundle() when Ic :: wxIconBundle:wxIconBundle() | wxIcon:wxIcon();
         (File) -> wxIconBundle() when File :: unicode:chardata().
```

Initializes the bundle with the icon(s) found in the file.

# `new`

```elixir
-spec new(File, Type) -> wxIconBundle() when File :: unicode:chardata(), Type :: wx:wx_enum().
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
