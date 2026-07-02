# `wxGridSizer`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxGridSizer.erl#L58)

A grid sizer is a sizer which lays out its children in a two-dimensional table with all
table fields having the same size, i.e.

the width of each field is the width of the widest child, the height of each field is the
height of the tallest child.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxSizer`

wxWidgets docs: [wxGridSizer](https://docs.wxwidgets.org/3.2/classwx_grid_sizer.html)

# `wxGridSizer`

```elixir
-type wxGridSizer() :: wx:wx_object().
```

# `destroy`

```elixir
-spec destroy(This :: wxGridSizer()) -> ok.
```

Destroys the object

# `getCols`

```elixir
-spec getCols(This) -> integer() when This :: wxGridSizer().
```

Returns the number of columns that has been specified for the sizer.

Returns zero if the sizer is automatically adjusting the number of columns depending on
number of its children. To get the effective number of columns or rows being currently
used, see `GetEffectiveColsCount()` (not implemented in wx)

# `getHGap`

```elixir
-spec getHGap(This) -> integer() when This :: wxGridSizer().
```

Returns the horizontal gap (in pixels) between cells in the sizer.

# `getRows`

```elixir
-spec getRows(This) -> integer() when This :: wxGridSizer().
```

Returns the number of rows that has been specified for the sizer.

Returns zero if the sizer is automatically adjusting the number of rows depending on
number of its children. To get the effective number of columns or rows being currently
used, see `GetEffectiveRowsCount()` (not implemented in wx).

# `getVGap`

```elixir
-spec getVGap(This) -> integer() when This :: wxGridSizer().
```

Returns the vertical gap (in pixels) between the cells in the sizer.

# `new`

```elixir
-spec new(Cols) -> wxGridSizer() when Cols :: integer().
```

# `new`

```elixir
-spec new(Cols, [Option]) -> wxGridSizer()
             when Cols :: integer(), Option :: {gap, {W :: integer(), H :: integer()}}.
```

# `new`

```elixir
-spec new(Cols, Vgap, Hgap) -> wxGridSizer()
             when Cols :: integer(), Vgap :: integer(), Hgap :: integer();
         (Rows, Cols, Gap) -> wxGridSizer()
             when Rows :: integer(), Cols :: integer(), Gap :: {W :: integer(), H :: integer()}.
```

# `new`

```elixir
-spec new(Rows, Cols, Vgap, Hgap) -> wxGridSizer()
             when Rows :: integer(), Cols :: integer(), Vgap :: integer(), Hgap :: integer().
```

# `setCols`

```elixir
-spec setCols(This, Cols) -> ok when This :: wxGridSizer(), Cols :: integer().
```

Sets the number of columns in the sizer.

# `setHGap`

```elixir
-spec setHGap(This, Gap) -> ok when This :: wxGridSizer(), Gap :: integer().
```

Sets the horizontal gap (in pixels) between cells in the sizer.

# `setRows`

```elixir
-spec setRows(This, Rows) -> ok when This :: wxGridSizer(), Rows :: integer().
```

Sets the number of rows in the sizer.

# `setVGap`

```elixir
-spec setVGap(This, Gap) -> ok when This :: wxGridSizer(), Gap :: integer().
```

Sets the vertical gap (in pixels) between the cells in the sizer.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
