# `wxGridBagSizer`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxGridBagSizer.erl#L58)

A `m:wxSizer` that can lay out items in a virtual grid like a `m:wxFlexGridSizer` but in
this case explicit positioning of the items is allowed using `wxGBPosition` (not
implemented in wx), and items can optionally span more than one row and/or column using `wxGBSpan`
(not implemented in wx).

This class is derived, and can use functions, from:

* `m:wxFlexGridSizer`

* `m:wxGridSizer`

* `m:wxSizer`

wxWidgets docs: [wxGridBagSizer](https://docs.wxwidgets.org/3.2/classwx_grid_bag_sizer.html)

# `wxGridBagSizer`

```erlang
-type wxGridBagSizer() :: wx:wx_object().
```

# `add`

```erlang
-spec add(This, Item) -> wxSizerItem:wxSizerItem()
             when This :: wxGridBagSizer(), Item :: wxGBSizerItem:wxGBSizerItem().
```

# `add`

```erlang
-spec add(This, Window, Pos) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxGridBagSizer(),
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                 Pos :: {R :: integer(), C :: integer()}.
```

# `add`

```erlang
-spec add(This, Width, Height, Pos) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxGridBagSizer(),
                 Width :: integer(),
                 Height :: integer(),
                 Pos :: {R :: integer(), C :: integer()};
         (This, Window, Pos, [Option]) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxGridBagSizer(),
                 Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                 Pos :: {R :: integer(), C :: integer()},
                 Option ::
                     {span, {RS :: integer(), CS :: integer()}} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()}.
```

Adds the given item to the given position.

Return: A valid pointer if the item was successfully placed at the given position, or
NULL if something was already there.

# `add`

```erlang
-spec add(This, Width, Height, Pos, [Option]) -> wxSizerItem:wxSizerItem()
             when
                 This :: wxGridBagSizer(),
                 Width :: integer(),
                 Height :: integer(),
                 Pos :: {R :: integer(), C :: integer()},
                 Option ::
                     {span, {RS :: integer(), CS :: integer()}} |
                     {flag, integer()} |
                     {border, integer()} |
                     {userData, wx:wx_object()}.
```

Adds a spacer to the given position.

`width` and `height` specify the dimension of the spacer to be added.

Return: A valid pointer if the spacer was successfully placed at the given position, or
NULL if something was already there.

# `calcMin`

```erlang
-spec calcMin(This) -> {W :: integer(), H :: integer()} when This :: wxGridBagSizer().
```

Called when the managed size of the sizer is needed or when layout needs done.

# `checkForIntersection`

```erlang
-spec checkForIntersection(This, Item) -> boolean()
                              when This :: wxGridBagSizer(), Item :: wxGBSizerItem:wxGBSizerItem().
```

# `checkForIntersection`

```erlang
-spec checkForIntersection(This, Pos, Span) -> boolean()
                              when
                                  This :: wxGridBagSizer(),
                                  Pos :: {R :: integer(), C :: integer()},
                                  Span :: {RS :: integer(), CS :: integer()};
                          (This, Item, [Option]) -> boolean()
                              when
                                  This :: wxGridBagSizer(),
                                  Item :: wxGBSizerItem:wxGBSizerItem(),
                                  Option :: {excludeItem, wxGBSizerItem:wxGBSizerItem()}.
```

Look at all items and see if any intersect (or would overlap) the given item.

Returns true if so, false if there would be no overlap. If an `excludeItem` is given then
it will not be checked for intersection, for example it may be the item we are checking
the position of.

# `checkForIntersection`

```erlang
-spec checkForIntersection(This, Pos, Span, [Option]) -> boolean()
                              when
                                  This :: wxGridBagSizer(),
                                  Pos :: {R :: integer(), C :: integer()},
                                  Span :: {RS :: integer(), CS :: integer()},
                                  Option :: {excludeItem, wxGBSizerItem:wxGBSizerItem()}.
```

# `destroy`

```erlang
-spec destroy(This :: wxGridBagSizer()) -> ok.
```

Destroys the object

# `findItem`

```erlang
-spec findItem(This, Window) -> wxGBSizerItem:wxGBSizerItem()
                  when This :: wxGridBagSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer().
```

Find the sizer item for the given window or subsizer, returns NULL if not found.

(non-recursive)

# `findItemAtPoint`

```erlang
-spec findItemAtPoint(This, Pt) -> wxGBSizerItem:wxGBSizerItem()
                         when This :: wxGridBagSizer(), Pt :: {X :: integer(), Y :: integer()}.
```

Return the sizer item located at the point given in pt, or NULL if there is no item at
that point.

The (x,y) coordinates in `pt` correspond to the client coordinates of the window using
the sizer for layout. (non-recursive)

# `findItemAtPosition`

```erlang
-spec findItemAtPosition(This, Pos) -> wxGBSizerItem:wxGBSizerItem()
                            when This :: wxGridBagSizer(), Pos :: {R :: integer(), C :: integer()}.
```

Return the sizer item for the given grid cell, or NULL if there is no item at that
position.

(non-recursive)

# `findItemWithData`

```erlang
-spec findItemWithData(This, UserData) -> wxGBSizerItem:wxGBSizerItem()
                          when This :: wxGridBagSizer(), UserData :: wx:wx_object().
```

Return the sizer item that has a matching user data (it only compares pointer values) or
NULL if not found.

(non-recursive)

# `getCellSize`

```erlang
-spec getCellSize(This, Row, Col) -> {W :: integer(), H :: integer()}
                     when This :: wxGridBagSizer(), Row :: integer(), Col :: integer().
```

Get the size of the specified cell, including hgap and vgap.

Only valid after window layout has been performed.

# `getEmptyCellSize`

```erlang
-spec getEmptyCellSize(This) -> {W :: integer(), H :: integer()} when This :: wxGridBagSizer().
```

Get the size used for cells in the grid with no item.

# `getItemPosition`

```erlang
-spec getItemPosition(This, Window) -> {R :: integer(), C :: integer()}
                         when
                             This :: wxGridBagSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
                     (This, Index) -> {R :: integer(), C :: integer()}
                         when This :: wxGridBagSizer(), Index :: integer().
```

# `getItemSpan`

```erlang
-spec getItemSpan(This, Window) -> {RS :: integer(), CS :: integer()}
                     when This :: wxGridBagSizer(), Window :: wxWindow:wxWindow() | wxSizer:wxSizer();
                 (This, Index) -> {RS :: integer(), CS :: integer()}
                     when This :: wxGridBagSizer(), Index :: integer().
```

# `new`

```erlang
-spec new() -> wxGridBagSizer().
```

# `new`

```erlang
-spec new([Option]) -> wxGridBagSizer() when Option :: {vgap, integer()} | {hgap, integer()}.
```

Constructor, with optional parameters to specify the gap between the rows and columns.

# `setEmptyCellSize`

```erlang
-spec setEmptyCellSize(This, Sz) -> ok
                          when This :: wxGridBagSizer(), Sz :: {W :: integer(), H :: integer()}.
```

Set the size used for cells in the grid with no item.

# `setItemPosition`

```erlang
-spec setItemPosition(This, Window, Pos) -> boolean()
                         when
                             This :: wxGridBagSizer(),
                             Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                             Pos :: {R :: integer(), C :: integer()};
                     (This, Index, Pos) -> boolean()
                         when
                             This :: wxGridBagSizer(),
                             Index :: integer(),
                             Pos :: {R :: integer(), C :: integer()}.
```

# `setItemSpan`

```erlang
-spec setItemSpan(This, Window, Span) -> boolean()
                     when
                         This :: wxGridBagSizer(),
                         Window :: wxWindow:wxWindow() | wxSizer:wxSizer(),
                         Span :: {RS :: integer(), CS :: integer()};
                 (This, Index, Span) -> boolean()
                     when
                         This :: wxGridBagSizer(),
                         Index :: integer(),
                         Span :: {RS :: integer(), CS :: integer()}.
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
