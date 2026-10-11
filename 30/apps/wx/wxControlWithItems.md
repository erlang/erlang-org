# `wxControlWithItems`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxControlWithItems.erl#L58)

This is convenience class that derives from both `m:wxControl` and `wxItemContainer` (not
implemented in wx).

It is used as basis for some wxWidgets controls (`m:wxChoice` and `m:wxListBox`).

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxControlWithItems](https://docs.wxwidgets.org/3.2/classwx_control_with_items.html)

# `wxControlWithItems`

```erlang
-type wxControlWithItems() :: wx:wx_object().
```

# `append`

```erlang
-spec append(This, Item) -> integer() when This :: wxControlWithItems(), Item :: unicode:chardata().
```

Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this may be
different from the last one if the control is sorted (e.g. has `wxLB_SORT` or `wxCB_SORT`
style).

# `append`

```erlang
-spec append(This, Item, ClientData) -> integer()
                when This :: wxControlWithItems(), Item :: unicode:chardata(), ClientData :: term().
```

Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this may be
different from the last one if the control is sorted (e.g. has `wxLB_SORT` or `wxCB_SORT`
style).

# `appendStrings`

```erlang
-spec appendStrings(This, Items) -> integer()
                       when This :: wxControlWithItems(), Items :: [unicode:chardata()].
```

Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one by one if
you need to add a lot of items.

# `appendStrings`

```erlang
-spec appendStrings(This, Items, ClientsData) -> integer()
                       when
                           This :: wxControlWithItems(),
                           Items :: [unicode:chardata()],
                           ClientsData :: [term()].
```

Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one by one if
you need to add a lot of items.

# `clear`

```erlang
-spec clear(This) -> ok when This :: wxControlWithItems().
```

Removes all items from the control.

`clear/1` also deletes the client data of the existing items if it is owned by the control.

# `delete`

```erlang
-spec delete(This, N) -> ok when This :: wxControlWithItems(), N :: integer().
```

Deletes an item from the control.

The client data associated with the item will be also deleted if it is owned by the
control. Note that it is an error (signalled by an assert failure in debug builds) to
remove an item with the index negative or greater or equal than the number of items in the control.

If there is a currently selected item below the item being deleted, i.e. if `getSelection/1` returns a
valid index greater than or equal to `n`, the selection is invalidated when this function
is called. However if the selected item appears before the item being deleted, the
selection is preserved unchanged.

See: `clear/1`

# `findString`

```erlang
-spec findString(This, String) -> integer()
                    when This :: wxControlWithItems(), String :: unicode:chardata().
```

# `findString`

```erlang
-spec findString(This, String, [Option]) -> integer()
                    when
                        This :: wxControlWithItems(),
                        String :: unicode:chardata(),
                        Option :: {bCase, boolean()}.
```

Finds an item whose label matches the given string.

Return: The zero-based position of the item, or wxNOT_FOUND if the string was not found.

# `getClientData`

```erlang
-spec getClientData(This, N) -> term() when This :: wxControlWithItems(), N :: integer().
```

Returns a pointer to the client data associated with the given item (if any).

It is an error to call this function for a control which doesn't have typed client data
at all although it is OK to call it even if the given item doesn't have any client data
associated with it (but other items do).

Notice that the returned pointer is still owned by the control and will be deleted by it,
use `DetachClientObject()` (not implemented in wx) if you want to remove the pointer from
the control.

Return: A pointer to the client data, or NULL if not present.

# `getCount`

```erlang
-spec getCount(This) -> integer() when This :: wxControlWithItems().
```

Returns the number of items in the control.

See: `isEmpty/1`

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxControlWithItems().
```

Returns the index of the selected item or `wxNOT\_FOUND` if no item is selected.

Return: The position of the current selection.

Remark: This method can be used with single selection list boxes only, you should use `wxListBox:getSelections/1`
for the list boxes with wxLB_MULTIPLE style.

See:
* `setSelection/2`

* `getStringSelection/1`

# `getString`

```erlang
-spec getString(This, N) -> unicode:charlist() when This :: wxControlWithItems(), N :: integer().
```

Returns the label of the item with the given index.

Return: The label of the item or an empty string if the position was invalid.

# `getStringSelection`

```erlang
-spec getStringSelection(This) -> unicode:charlist() when This :: wxControlWithItems().
```

Returns the label of the selected item or an empty string if no item is selected.

See: `getSelection/1`

# `insert`

```erlang
-spec insert(This, Item, Pos) -> integer()
                when This :: wxControlWithItems(), Item :: unicode:chardata(), Pos :: integer().
```

Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the insertion failed
for some reason, -1 is returned.

# `insert`

```erlang
-spec insert(This, Item, Pos, ClientData) -> integer()
                when
                    This :: wxControlWithItems(),
                    Item :: unicode:chardata(),
                    Pos :: integer(),
                    ClientData :: term().
```

Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the insertion failed
for some reason, -1 is returned.

# `insertStrings`

```erlang
-spec insertStrings(This, Items, Pos) -> integer()
                       when
                           This :: wxControlWithItems(), Items :: [unicode:chardata()], Pos :: integer().
```

Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one by one if
you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the insertion failed
for some reason, -1 is returned.

# `insertStrings`

```erlang
-spec insertStrings(This, Items, Pos, ClientsData) -> integer()
                       when
                           This :: wxControlWithItems(),
                           Items :: [unicode:chardata()],
                           Pos :: integer(),
                           ClientsData :: [term()].
```

Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one by one if
you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the insertion failed
for some reason, -1 is returned.

# `isEmpty`

```erlang
-spec isEmpty(This) -> boolean() when This :: wxControlWithItems().
```

Returns true if the control is empty or false if it has some items.

See: `getCount/1`

# `select`

```erlang
-spec select(This, N) -> ok when This :: wxControlWithItems(), N :: integer().
```

This is the same as `setSelection/2` and exists only because it is slightly more natural
for controls which support multiple selection.

# `setClientData`

```erlang
-spec setClientData(This, N, Data) -> ok
                       when This :: wxControlWithItems(), N :: integer(), Data :: term().
```

Associates the given typed client data pointer with the given item: the `data` object
will be deleted when the item is deleted (either explicitly by using `delete/2` or
implicitly when the control itself is destroyed).

Note that it is an error to call this function if any untyped client data pointers had
been associated with the control items before.

# `setSelection`

```erlang
-spec setSelection(This, N) -> ok when This :: wxControlWithItems(), N :: integer().
```

Sets the selection to the given item `n` or removes the selection entirely if `n` == `wxNOT\_FOUND`.

Note that this does not cause any command events to be emitted nor does it deselect any
other items in the controls which support multiple selections.

See:
* `setString/3`

* `setStringSelection/2`

# `setString`

```erlang
-spec setString(This, N, String) -> ok
                   when This :: wxControlWithItems(), N :: integer(), String :: unicode:chardata().
```

Sets the label for the given item.

# `setStringSelection`

```erlang
-spec setStringSelection(This, String) -> boolean()
                            when This :: wxControlWithItems(), String :: unicode:chardata().
```

Selects the item with the specified string in the control.

This method doesn't cause any command events to be emitted.

Notice that this method is case-insensitive, i.e. the string is compared with all the
elements of the control case-insensitively and the first matching entry is selected, even
if it doesn't have exactly the same case as this string and there is an exact match afterwards.

Return: true if the specified string has been selected, false if it wasn't found in the
control.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
