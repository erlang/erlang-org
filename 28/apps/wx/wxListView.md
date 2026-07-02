# `wxListView`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxListView.erl#L58)

This class currently simply presents a simpler to use interface for the `m:wxListCtrl` --
it can be thought of as a `façade` for that complicated class.

Using it is preferable to using `m:wxListCtrl` directly whenever possible because in the
future some ports might implement `m:wxListView` but not the full set of `m:wxListCtrl` features.

Other than different interface, this class is identical to `m:wxListCtrl`. In particular,
it uses the same events, same window styles and so on.

See: `setColumnImage/3`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxListView](https://docs.wxwidgets.org/3.2/classwx_list_view.html)

# `wxListView`

```elixir
-type wxListView() :: wx:wx_object().
```

# `clearColumnImage`

```elixir
-spec clearColumnImage(This, Col) -> ok when This :: wxListView(), Col :: integer().
```

Resets the column image -- after calling this function, no image will be shown.

See: `setColumnImage/3`

# `focus`

```elixir
-spec focus(This, Index) -> ok when This :: wxListView(), Index :: integer().
```

Sets focus to the item with the given `index`.

# `getFirstSelected`

```elixir
-spec getFirstSelected(This) -> integer() when This :: wxListView().
```

Returns the first selected item in a (presumably) multiple selection control.

Together with `getNextSelected/2` it can be used to iterate over all selected items in the control.

Return: The first selected item, if any, -1 otherwise.

# `getFocusedItem`

```elixir
-spec getFocusedItem(This) -> integer() when This :: wxListView().
```

Returns the currently focused item or -1 if none.

See:
* `isSelected/2`

* `focus/2`

# `getNextSelected`

```elixir
-spec getNextSelected(This, Item) -> integer() when This :: wxListView(), Item :: integer().
```

Used together with `getFirstSelected/1` to iterate over all selected items in the
control.

Return: Returns the next selected item or -1 if there are no more of them.

# `isSelected`

```elixir
-spec isSelected(This, Index) -> boolean() when This :: wxListView(), Index :: integer().
```

Returns true if the item with the given `index` is selected, false otherwise.

See:
* `getFirstSelected/1`

* `getNextSelected/2`

# `select`

```elixir
-spec select(This, N) -> ok when This :: wxListView(), N :: integer().
```

# `select`

```elixir
-spec select(This, N, [Option]) -> ok
                when This :: wxListView(), N :: integer(), Option :: {on, boolean()}.
```

Selects or unselects the given item.

Notice that this method inherits the unusual behaviour of `wxListCtrl:setItemState/4` which sends a
wxEVT_LIST_ITEM_SELECTED event when it is used to select an item, contrary to the usual
rule that only the user actions result in selection.

# `setColumnImage`

```elixir
-spec setColumnImage(This, Col, Image) -> ok
                        when This :: wxListView(), Col :: integer(), Image :: integer().
```

Sets the column image for the specified column.

To use the column images, the control must have a valid image list with at least one image.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
