# `wxListEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxListEvent.erl#L58)

A list event holds information about events associated with `m:wxListCtrl` objects.

See: `m:wxListCtrl`

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxListEvent](https://docs.wxwidgets.org/3.2/classwx_list_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxListEventType` to subscribe to events of this type.

# `wxList`

```erlang
-type wxList() ::
          #wxList{type :: wxListEvent:wxListEventType(),
                  code :: integer(),
                  oldItemIndex :: integer(),
                  itemIndex :: integer(),
                  col :: integer(),
                  pointDrag :: {X :: integer(), Y :: integer()}}.
```

# `wxListEvent`

```erlang
-type wxListEvent() :: wx:wx_object().
```

# `wxListEventType`

```erlang
-type wxListEventType() ::
          command_list_begin_drag | command_list_begin_rdrag | command_list_begin_label_edit |
          command_list_end_label_edit | command_list_delete_item | command_list_delete_all_items |
          command_list_key_down | command_list_insert_item | command_list_col_click |
          command_list_col_right_click | command_list_col_begin_drag | command_list_col_dragging |
          command_list_col_end_drag | command_list_item_selected | command_list_item_deselected |
          command_list_item_right_click | command_list_item_middle_click | command_list_item_activated |
          command_list_item_focused | command_list_cache_hint.
```

# `getCacheFrom`

```erlang
-spec getCacheFrom(This) -> integer() when This :: wxListEvent().
```

For `EVT\_LIST\_CACHE\_HINT` event only: return the first item which the list control
advises us to cache.

# `getCacheTo`

```erlang
-spec getCacheTo(This) -> integer() when This :: wxListEvent().
```

For `EVT\_LIST\_CACHE\_HINT` event only: return the last item (inclusive) which the list
control advises us to cache.

# `getColumn`

```erlang
-spec getColumn(This) -> integer() when This :: wxListEvent().
```

The column position: it is only used with `COL` events.

For the column dragging events, it is the column to the left of the divider being
dragged, for the column click events it may be -1 if the user clicked in the list control
header outside any column.

# `getData`

```erlang
-spec getData(This) -> integer() when This :: wxListEvent().
```

The data.

# `getImage`

```erlang
-spec getImage(This) -> integer() when This :: wxListEvent().
```

The image.

# `getIndex`

```erlang
-spec getIndex(This) -> integer() when This :: wxListEvent().
```

The item index.

# `getItem`

```erlang
-spec getItem(This) -> wxListItem:wxListItem() when This :: wxListEvent().
```

An item object, used by some events.

See also `wxListCtrl:setItem/5`.

# `getKeyCode`

```erlang
-spec getKeyCode(This) -> integer() when This :: wxListEvent().
```

Key code if the event is a keypress event.

# `getLabel`

```erlang
-spec getLabel(This) -> unicode:charlist() when This :: wxListEvent().
```

The (new) item label for `EVT_LIST_END_LABEL_EDIT` event.

# `getMask`

```erlang
-spec getMask(This) -> integer() when This :: wxListEvent().
```

The mask.

# `getPoint`

```erlang
-spec getPoint(This) -> {X :: integer(), Y :: integer()} when This :: wxListEvent().
```

The position of the mouse pointer if the event is a drag event.

# `getText`

```erlang
-spec getText(This) -> unicode:charlist() when This :: wxListEvent().
```

The text.

# `isEditCancelled`

```erlang
-spec isEditCancelled(This) -> boolean() when This :: wxListEvent().
```

This method only makes sense for `EVT\_LIST\_END\_LABEL\_EDIT` message and returns true
if it the label editing has been cancelled by the user (`getLabel/1` returns an empty
string in this case but it doesn't allow the application to distinguish between really
cancelling the edit and the admittedly rare case when the user wants to rename it to an
empty string).

---

*Consult [api-reference.md](api-reference.md) for complete listing*
