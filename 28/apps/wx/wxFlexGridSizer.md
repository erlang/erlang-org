# `wxFlexGridSizer`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxFlexGridSizer.erl#L58)

A flex grid sizer is a sizer which lays out its children in a two-dimensional table with
all table fields in one row having the same height and all fields in one column having the
same width, but all rows or all columns are not necessarily the same height or width as in
the `m:wxGridSizer`.

Since wxWidgets 2.5.0, `m:wxFlexGridSizer` can also size items equally in one direction
but unequally ("flexibly") in the other. If the sizer is only flexible in one direction
(this can be changed using `setFlexibleDirection/2`), it needs to be decided how the sizer should grow in the other
("non-flexible") direction in order to fill the available space. The `setNonFlexibleGrowMode/2` method serves this purpose.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxGridSizer`

* `m:wxSizer`

wxWidgets docs: [wxFlexGridSizer](https://docs.wxwidgets.org/3.2/classwx_flex_grid_sizer.html)

# `wxFlexGridSizer`

```elixir
-type wxFlexGridSizer() :: wx:wx_object().
```

# `addGrowableCol`

```elixir
-spec addGrowableCol(This, Idx) -> ok when This :: wxFlexGridSizer(), Idx :: integer().
```

# `addGrowableCol`

```elixir
-spec addGrowableCol(This, Idx, [Option]) -> ok
                        when
                            This :: wxFlexGridSizer(),
                            Idx :: integer(),
                            Option :: {proportion, integer()}.
```

Specifies that column `idx` (starting from zero) should be grown if there is extra space
available to the sizer.

The `proportion` parameter has the same meaning as the stretch factor for the sizers (see `m:wxBoxSizer`)
except that if all proportions are 0, then all columns are resized equally (instead of not
being resized at all).

Notice that the column must not be already growable, if you need to change the proportion
you must call `removeGrowableCol/2` first and then make it growable (with a different proportion) again. You
can use `IsColGrowable()` (not implemented in wx) to check whether a column is already
growable.

# `addGrowableRow`

```elixir
-spec addGrowableRow(This, Idx) -> ok when This :: wxFlexGridSizer(), Idx :: integer().
```

# `addGrowableRow`

```elixir
-spec addGrowableRow(This, Idx, [Option]) -> ok
                        when
                            This :: wxFlexGridSizer(),
                            Idx :: integer(),
                            Option :: {proportion, integer()}.
```

Specifies that row idx (starting from zero) should be grown if there is extra space
available to the sizer.

This is identical to `addGrowableCol/3` except that it works with rows and not columns.

# `destroy`

```elixir
-spec destroy(This :: wxFlexGridSizer()) -> ok.
```

Destroys the object

# `getFlexibleDirection`

```elixir
-spec getFlexibleDirection(This) -> integer() when This :: wxFlexGridSizer().
```

Returns a ?wxOrientation value that specifies whether the sizer flexibly resizes its
columns, rows, or both (default).

Return: One of the following values:

* wxVERTICAL: Rows are flexibly sized.

* wxHORIZONTAL: Columns are flexibly sized.

* wxBOTH: Both rows and columns are flexibly sized (this is the default value).

See: `setFlexibleDirection/2`

# `getNonFlexibleGrowMode`

```elixir
-spec getNonFlexibleGrowMode(This) -> wx:wx_enum() when This :: wxFlexGridSizer().
```

Returns the value that specifies how the sizer grows in the "non-flexible" direction if
there is one.

The behaviour of the elements in the flexible direction (i.e. both rows and columns by
default, or rows only if `getFlexibleDirection/1` is `wxVERTICAL` or columns only if it is `wxHORIZONTAL`) is
always governed by their proportion as specified in the call to `addGrowableRow/3` or `addGrowableCol/3`. What happens in the
other direction depends on the value of returned by this function as described below.

Return: One of the following values:

* wxFLEX_GROWMODE_NONE: Sizer doesn't grow its elements at all in the non-flexible direction.

* wxFLEX_GROWMODE_SPECIFIED: Sizer honors growable columns/rows set with `addGrowableCol/3` and `addGrowableRow/3` in the
non-flexible direction as well. In this case equal sizing applies to minimum sizes of
columns or rows (this is the default value).

* wxFLEX_GROWMODE_ALL: Sizer equally stretches all columns or rows in the non-flexible
direction, independently of the proportions applied in the flexible direction.

See:
* `setFlexibleDirection/2`

* `setNonFlexibleGrowMode/2`

# `new`

```elixir
-spec new(Cols) -> wxFlexGridSizer() when Cols :: integer().
```

# `new`

```elixir
-spec new(Cols, [Option]) -> wxFlexGridSizer()
             when Cols :: integer(), Option :: {gap, {W :: integer(), H :: integer()}}.
```

# `new`

```elixir
-spec new(Cols, Vgap, Hgap) -> wxFlexGridSizer()
             when Cols :: integer(), Vgap :: integer(), Hgap :: integer();
         (Rows, Cols, Gap) -> wxFlexGridSizer()
             when Rows :: integer(), Cols :: integer(), Gap :: {W :: integer(), H :: integer()}.
```

# `new`

```elixir
-spec new(Rows, Cols, Vgap, Hgap) -> wxFlexGridSizer()
             when Rows :: integer(), Cols :: integer(), Vgap :: integer(), Hgap :: integer().
```

# `removeGrowableCol`

```elixir
-spec removeGrowableCol(This, Idx) -> ok when This :: wxFlexGridSizer(), Idx :: integer().
```

Specifies that the `idx` column index is no longer growable.

# `removeGrowableRow`

```elixir
-spec removeGrowableRow(This, Idx) -> ok when This :: wxFlexGridSizer(), Idx :: integer().
```

Specifies that the `idx` row index is no longer growable.

# `setFlexibleDirection`

```elixir
-spec setFlexibleDirection(This, Direction) -> ok when This :: wxFlexGridSizer(), Direction :: integer().
```

Specifies whether the sizer should flexibly resize its columns, rows, or both.

Argument `direction` can be `wxVERTICAL`, `wxHORIZONTAL` or `wxBOTH` (which is the
default value). Any other value is ignored.

See `getFlexibleDirection/1` for the explanation of these values. Note that this method does not trigger
relayout.

# `setNonFlexibleGrowMode`

```elixir
-spec setNonFlexibleGrowMode(This, Mode) -> ok when This :: wxFlexGridSizer(), Mode :: wx:wx_enum().
```

Specifies how the sizer should grow in the non-flexible direction if there is one (so `setFlexibleDirection/2`
must have been called previously).

Argument `mode` can be one of those documented in `getNonFlexibleGrowMode/1`, please see there for their
explanation. Note that this method does not trigger relayout.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
