# `wxGridCellAttr`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGridCellAttr.erl#L58)

This class can be used to alter the cells' appearance in the grid by changing their
attributes from the defaults.

An object of this class may be returned by `wxGridTableBase::GetAttr()` (not implemented
in wx).

Note that objects of this class are reference-counted and it's recommended to use
wxGridCellAttrPtr smart pointer class when working with them to avoid memory leaks.

wxWidgets docs: [wxGridCellAttr](https://docs.wxwidgets.org/3.2/classwx_grid_cell_attr.html)

# `wxGridCellAttr`

```elixir
-type wxGridCellAttr() :: wx:wx_object().
```

# `getAlignment`

```elixir
-spec getAlignment(This) -> {HAlign :: integer(), VAlign :: integer()} when This :: wxGridCellAttr().
```

Get the alignment to use for the cell with the given attribute.

If this attribute doesn't specify any alignment, the default attribute alignment is used
(which can be changed using `wxGrid:setDefaultCellAlignment/3` but is left and top by default).

Notice that `hAlign` and `vAlign` values are always overwritten by this function, use `GetNonDefaultAlignment()`
(not implemented in wx) if this is not desirable.

# `getBackgroundColour`

```elixir
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxGridCellAttr().
```

Returns the background colour.

# `getEditor`

```elixir
-spec getEditor(This, Grid, Row, Col) -> wxGridCellEditor:wxGridCellEditor()
                   when
                       This :: wxGridCellAttr(),
                       Grid :: wxGrid:wxGrid(),
                       Row :: integer(),
                       Col :: integer().
```

Returns the cell editor.

The caller is responsible for calling `DecRef()` (not implemented in wx) on the returned
pointer, use `GetEditorPtr()` (not implemented in wx) to do it automatically.

# `getFont`

```elixir
-spec getFont(This) -> wxFont:wxFont() when This :: wxGridCellAttr().
```

Returns the font.

# `getRenderer`

```elixir
-spec getRenderer(This, Grid, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer()
                     when
                         This :: wxGridCellAttr(),
                         Grid :: wxGrid:wxGrid(),
                         Row :: integer(),
                         Col :: integer().
```

Returns the cell renderer.

The caller is responsible for calling `DecRef()` (not implemented in wx) on the returned
pointer, use `GetRendererPtr()` (not implemented in wx) to do it automatically.

# `getTextColour`

```elixir
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxGridCellAttr().
```

Returns the text colour.

# `hasAlignment`

```elixir
-spec hasAlignment(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid alignment set.

# `hasBackgroundColour`

```elixir
-spec hasBackgroundColour(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid background colour set.

# `hasEditor`

```elixir
-spec hasEditor(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid cell editor set.

# `hasFont`

```elixir
-spec hasFont(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid font set.

# `hasRenderer`

```elixir
-spec hasRenderer(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid cell renderer set.

# `hasTextColour`

```elixir
-spec hasTextColour(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this attribute has a valid text colour set.

# `isReadOnly`

```elixir
-spec isReadOnly(This) -> boolean() when This :: wxGridCellAttr().
```

Returns true if this cell is set as read-only.

# `setAlignment`

```elixir
-spec setAlignment(This, HAlign, VAlign) -> ok
                      when This :: wxGridCellAttr(), HAlign :: integer(), VAlign :: integer().
```

Sets the alignment.

`hAlign` can be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or `wxALIGN_RIGHT` and `vAlign`
can be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.

# `setBackgroundColour`

```elixir
-spec setBackgroundColour(This, ColBack) -> ok when This :: wxGridCellAttr(), ColBack :: wx:wx_colour().
```

Sets the background colour.

# `setDefAttr`

```elixir
-spec setDefAttr(This, DefAttr) -> ok when This :: wxGridCellAttr(), DefAttr :: wxGridCellAttr().
```

# `setEditor`

```elixir
-spec setEditor(This, Editor) -> ok
                   when This :: wxGridCellAttr(), Editor :: wxGridCellEditor:wxGridCellEditor().
```

Sets the editor to be used with the cells with this attribute.

# `setFont`

```elixir
-spec setFont(This, Font) -> ok when This :: wxGridCellAttr(), Font :: wxFont:wxFont().
```

Sets the font.

# `setReadOnly`

```elixir
-spec setReadOnly(This) -> ok when This :: wxGridCellAttr().
```

# `setReadOnly`

```elixir
-spec setReadOnly(This, [Option]) -> ok when This :: wxGridCellAttr(), Option :: {isReadOnly, boolean()}.
```

Sets the cell as read-only.

# `setRenderer`

```elixir
-spec setRenderer(This, Renderer) -> ok
                     when This :: wxGridCellAttr(), Renderer :: wxGridCellRenderer:wxGridCellRenderer().
```

Sets the renderer to be used for cells with this attribute.

Takes ownership of the pointer.

# `setTextColour`

```elixir
-spec setTextColour(This, ColText) -> ok when This :: wxGridCellAttr(), ColText :: wx:wx_colour().
```

Sets the text colour.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
