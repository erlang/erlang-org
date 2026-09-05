# `wxTreeEvent`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxTreeEvent.erl#L58)

A tree event holds information about events associated with `m:wxTreeCtrl` objects.

To process input from a tree control, use these event handler macros to direct input to
member functions that take a `m:wxTreeEvent` argument.

See: `m:wxTreeCtrl`

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxTreeEvent](https://docs.wxwidgets.org/3.2/classwx_tree_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxTreeEventType` to subscribe to events of this type.

# `wxTree`

```erlang
-type wxTree() ::
          #wxTree{type :: wxTreeEvent:wxTreeEventType(),
                  item :: integer(),
                  itemOld :: integer(),
                  pointDrag :: {X :: integer(), Y :: integer()}}.
```

# `wxTreeEvent`

```erlang
-type wxTreeEvent() :: wx:wx_object().
```

# `wxTreeEventType`

```erlang
-type wxTreeEventType() ::
          command_tree_begin_drag | command_tree_begin_rdrag | command_tree_begin_label_edit |
          command_tree_end_label_edit | command_tree_delete_item | command_tree_get_info |
          command_tree_set_info | command_tree_item_expanded | command_tree_item_expanding |
          command_tree_item_collapsed | command_tree_item_collapsing | command_tree_sel_changed |
          command_tree_sel_changing | command_tree_key_down | command_tree_item_activated |
          command_tree_item_right_click | command_tree_item_middle_click | command_tree_end_drag |
          command_tree_state_image_click | command_tree_item_gettooltip | command_tree_item_menu |
          dirctrl_selectionchanged | dirctrl_fileactivated.
```

# `getItem`

```erlang
-spec getItem(This) -> integer() when This :: wxTreeEvent().
```

Returns the item (valid for all events).

# `getKeyCode`

```erlang
-spec getKeyCode(This) -> integer() when This :: wxTreeEvent().
```

Returns the key code if the event is a key event.

Use `getKeyEvent/1` to get the values of the modifier keys for this event (i.e. Shift or Ctrl).

# `getKeyEvent`

```erlang
-spec getKeyEvent(This) -> wxKeyEvent:wxKeyEvent() when This :: wxTreeEvent().
```

Returns the key event for `EVT_TREE_KEY_DOWN` events.

# `getLabel`

```erlang
-spec getLabel(This) -> unicode:charlist() when This :: wxTreeEvent().
```

Returns the label if the event is a begin or end edit label event.

# `getOldItem`

```erlang
-spec getOldItem(This) -> integer() when This :: wxTreeEvent().
```

Returns the old item index (valid for `EVT\_TREE\_SEL\_CHANGING` and `EVT\_TREE\_SEL\_CHANGED`
events).

# `getPoint`

```erlang
-spec getPoint(This) -> {X :: integer(), Y :: integer()} when This :: wxTreeEvent().
```

Returns the position of the mouse pointer if the event is a drag or menu-context event.

In both cases the position is in client coordinates - i.e. relative to the `m:wxTreeCtrl`
window (so that you can pass it directly to e.g. `wxWindow:popupMenu/4`).

# `isEditCancelled`

```erlang
-spec isEditCancelled(This) -> boolean() when This :: wxTreeEvent().
```

Returns true if the label edit was cancelled.

This should be called from within an `EVT_TREE_END_LABEL_EDIT` handler.

# `setToolTip`

```erlang
-spec setToolTip(This, Tooltip) -> ok when This :: wxTreeEvent(), Tooltip :: unicode:chardata().
```

Set the tooltip for the item (valid for `EVT\_TREE\_ITEM\_GETTOOLTIP` events).

Windows only.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
