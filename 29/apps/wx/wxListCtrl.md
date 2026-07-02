# `wxListCtrl`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxListCtrl.erl#L58)

A list control presents lists in a number of formats: list view, report view, icon view
and small icon view.

In any case, elements are numbered from zero. For all these modes, the items are stored
in the control and must be added to it using `insertItem/4` method.

A special case of report view quite different from the other modes of the list control is
a virtual control in which the items data (including text, images and attributes) is
managed by the main program and is requested by the control itself only when needed which
allows having controls with millions of items without consuming much memory. To use
virtual list control you must use `setItemCount/2` first and override at least `wxListCtrl::OnGetItemText`
(not implemented in wx) (and optionally `wxListCtrl::OnGetItemImage` (not implemented in
wx) or `wxListCtrl::OnGetItemColumnImage` (not implemented in wx) and `wxListCtrl::OnGetItemAttr`
(not implemented in wx)) to return the information about the items when the control
requests it.

Virtual list control can be used as a normal one except that no operations which can take
time proportional to the number of items in the control happen - this is required to allow
having a practically infinite number of items. For example, in a multiple selection
virtual list control, the selections won't be sent when many items are selected at once
because this could mean iterating over all the items.

Using many of `m:wxListCtrl` features is shown in the corresponding sample.

To intercept events from a list control, use the event table macros described in `m:wxListEvent`.

`wxMac Note`: Starting with wxWidgets 2.8, `m:wxListCtrl` uses a native implementation
for report mode, and uses a generic implementation for other modes. You can use the
generic implementation for report mode as well by setting the `mac.listctrl.always_use_generic`
system option (see `m:wxSystemOptions`) to 1.

## Styles

This class supports the following styles:

* wxLC_LIST: Multicolumn list view, with optional small icons. Columns are computed
automatically, i.e. you don't set columns as in `wxLC_REPORT`. In other words, the list
wraps, unlike a `m:wxListBox`.

* wxLC_REPORT: Single or multicolumn report view, with optional header.

* wxLC_VIRTUAL: The application provides items text on demand. May only be used with `wxLC_REPORT`.

* wxLC_ICON: Large icon view, with optional labels.

* wxLC_SMALL_ICON: Small icon view, with optional labels.

* wxLC_ALIGN_TOP: Icons align to the top. Win32 default, Win32 only.

* wxLC_ALIGN_LEFT: Icons align to the left.

* wxLC_AUTOARRANGE: Icons arrange themselves. Win32 only.

* wxLC_EDIT_LABELS: Labels are editable: the application will be notified when editing
starts.

* wxLC_NO_HEADER: No header in report mode.

* wxLC_SINGLE_SEL: Single selection (default is multiple).

* wxLC_SORT_ASCENDING: Sort in ascending order. (You must still supply a comparison
callback in `sortItems/2`.)

* wxLC_SORT_DESCENDING: Sort in descending order. (You must still supply a comparison
callback in `sortItems/2`.)

* wxLC_HRULES: Draws light horizontal rules between rows in report mode.

* wxLC_VRULES: Draws light vertical rules between columns in report mode.

See:
* [Overview listctrl](https://docs.wxwidgets.org/3.2/overview_listctrl.html#overview_listctrl)

* `m:wxListView`

* `m:wxListBox`

* `m:wxTreeCtrl`

* `m:wxImageList`

* `m:wxListEvent`

* `m:wxListItem`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxListCtrl](https://docs.wxwidgets.org/3.2/classwx_list_ctrl.html)

## Events

Event types emitted from this class:

* [`command_list_begin_drag`](`m:wxListEvent`)

* [`command_list_begin_rdrag`](`m:wxListEvent`)

* [`command_list_begin_label_edit`](`m:wxListEvent`)

* [`command_list_end_label_edit`](`m:wxListEvent`)

* [`command_list_delete_item`](`m:wxListEvent`)

* [`command_list_delete_all_items`](`m:wxListEvent`)

* [`command_list_item_selected`](`m:wxListEvent`)

* [`command_list_item_deselected`](`m:wxListEvent`)

* [`command_list_item_activated`](`m:wxListEvent`)

* [`command_list_item_focused`](`m:wxListEvent`)

* [`command_list_item_middle_click`](`m:wxListEvent`)

* [`command_list_item_right_click`](`m:wxListEvent`)

* [`command_list_key_down`](`m:wxListEvent`)

* [`command_list_insert_item`](`m:wxListEvent`)

* [`command_list_col_click`](`m:wxListEvent`)

* [`command_list_col_right_click`](`m:wxListEvent`)

* [`command_list_col_begin_drag`](`m:wxListEvent`)

* [`command_list_col_dragging`](`m:wxListEvent`)

* [`command_list_col_end_drag`](`m:wxListEvent`)

* [`command_list_cache_hint`](`m:wxListEvent`)

# `wxListCtrl`

```erlang
-type wxListCtrl() :: wx:wx_object().
```

# `arrange`

```erlang
-spec arrange(This) -> boolean() when This :: wxListCtrl().
```

# `arrange`

```erlang
-spec arrange(This, [Option]) -> boolean() when This :: wxListCtrl(), Option :: {flag, integer()}.
```

Arranges the items in icon or small icon view.

This only has effect on Win32. `flag` is one of:

* wxLIST_ALIGN_DEFAULT: Default alignment.

* wxLIST_ALIGN_LEFT: Align to the left side of the control.

* wxLIST_ALIGN_TOP: Align to the top side of the control.

* wxLIST_ALIGN_SNAP_TO_GRID: Snap to grid.

# `assignImageList`

```erlang
-spec assignImageList(This, ImageList, Which) -> ok
                         when
                             This :: wxListCtrl(),
                             ImageList :: wxImageList:wxImageList(),
                             Which :: integer().
```

Sets the image list associated with the control and takes ownership of it (i.e.

the control will, unlike when using `setImageList/3`, delete the list when destroyed). `which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`, `wxIMAGE_LIST_STATE`
(the last is unimplemented).

See: `setImageList/3`

# `clearAll`

```erlang
-spec clearAll(This) -> ok when This :: wxListCtrl().
```

Deletes all items and all columns.

Note: This sends an event of type `wxEVT_LIST_DELETE_ALL_ITEMS` under all platforms.

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxWindow:wxWindow(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxWindow:wxWindow(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {winid, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()} |
                        {onGetItemText, function()} |
                        {onGetItemAttr, function()} |
                        {onGetItemColumnImage, function()}.
```

# `deleteAllItems`

```erlang
-spec deleteAllItems(This) -> boolean() when This :: wxListCtrl().
```

Deletes all items in the list control.

This function does `not` send the `wxEVT_LIST_DELETE_ITEM` event because deleting many
items from the control would be too slow then (unlike `deleteItem/2`) but it does send the special `wxEVT_LIST_DELETE_ALL_ITEMS`
event if the control was not empty. If it was already empty, nothing is done and no event
is sent.

Return: true if the items were successfully deleted or if the control was already empty,
false if an error occurred while deleting the items.

# `deleteColumn`

```erlang
-spec deleteColumn(This, Col) -> boolean() when This :: wxListCtrl(), Col :: integer().
```

Deletes a column.

# `deleteItem`

```erlang
-spec deleteItem(This, Item) -> boolean() when This :: wxListCtrl(), Item :: integer().
```

Deletes the specified item.

This function sends the `wxEVT_LIST_DELETE_ITEM` event for the item being deleted.

See: `deleteAllItems/1`

# `destroy`

```erlang
-spec destroy(This :: wxListCtrl()) -> ok.
```

Destroys the object

# `editLabel`

```erlang
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when This :: wxListCtrl(), Item :: integer().
```

Starts editing the label of the given item.

This function generates a `EVT_LIST_BEGIN_LABEL_EDIT` event which can be vetoed so that
no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text control
without changes, a `EVT_LIST_END_LABEL_EDIT` event will be sent which can be vetoed as
well.

# `ensureVisible`

```erlang
-spec ensureVisible(This, Item) -> boolean() when This :: wxListCtrl(), Item :: integer().
```

Ensures this item is visible.

# `findItem`

```erlang
-spec findItem(This, Start, Str) -> integer()
                  when This :: wxListCtrl(), Start :: integer(), Str :: unicode:chardata().
```

# `findItem`

```erlang
-spec findItem(This, Start, Str, [Option]) -> integer()
                  when
                      This :: wxListCtrl(),
                      Start :: integer(),
                      Str :: unicode:chardata(),
                      Option :: {partial, boolean()};
              (This, Start, Pt, Direction) -> integer()
                  when
                      This :: wxListCtrl(),
                      Start :: integer(),
                      Pt :: {X :: integer(), Y :: integer()},
                      Direction :: integer().
```

Find an item nearest this position in the specified direction, starting from `start` or
the beginning if `start` is -1.

Return: The next matching item if any or `-1` (wxNOT_FOUND) otherwise.

# `getColumn`

```erlang
-spec getColumn(This, Col, Item) -> boolean()
                   when This :: wxListCtrl(), Col :: integer(), Item :: wxListItem:wxListItem().
```

Gets information about this column.

See `setItem/5` for more information.

# `getColumnCount`

```erlang
-spec getColumnCount(This) -> integer() when This :: wxListCtrl().
```

Returns the number of columns.

# `getColumnWidth`

```erlang
-spec getColumnWidth(This, Col) -> integer() when This :: wxListCtrl(), Col :: integer().
```

Gets the column width (report view only).

# `getCountPerPage`

```erlang
-spec getCountPerPage(This) -> integer() when This :: wxListCtrl().
```

Gets the number of items that can fit vertically in the visible area of the list control
(list or report view) or the total number of items in the list control (icon or small icon
view).

# `getEditControl`

```erlang
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when This :: wxListCtrl().
```

Returns the edit control being currently used to edit a label.

Returns NULL if no label is being edited.

Note: It is currently only implemented for wxMSW and the generic version, not for the
native macOS version.

# `getImageList`

```erlang
-spec getImageList(This, Which) -> wxImageList:wxImageList()
                      when This :: wxListCtrl(), Which :: integer().
```

Returns the specified image list.

`which` may be one of:

* wxIMAGE_LIST_NORMAL: The normal (large icon) image list.

* wxIMAGE_LIST_SMALL: The small icon image list.

* wxIMAGE_LIST_STATE: The user-defined state image list (unimplemented).

# `getItem`

```erlang
-spec getItem(This, Info) -> boolean() when This :: wxListCtrl(), Info :: wxListItem:wxListItem().
```

Gets information about the item.

See `setItem/5` for more information.

You must call `info.SetId()` to set the ID of item you're interested in before calling
this method, and `info.SetMask()` with the flags indicating what fields you need to
retrieve from `info`.

# `getItemBackgroundColour`

```erlang
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4()
                                 when This :: wxListCtrl(), Item :: integer().
```

Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the default
background control of the control itself).

See: `getItemTextColour/2`

# `getItemCount`

```erlang
-spec getItemCount(This) -> integer() when This :: wxListCtrl().
```

Returns the number of items in the list control.

# `getItemData`

```erlang
-spec getItemData(This, Item) -> integer() when This :: wxListCtrl(), Item :: integer().
```

Gets the application-defined data associated with this item.

# `getItemFont`

```erlang
-spec getItemFont(This, Item) -> wxFont:wxFont() when This :: wxListCtrl(), Item :: integer().
```

Returns the item's font.

# `getItemPosition`

```erlang
-spec getItemPosition(This, Item) -> Result
                         when
                             Result :: {Res :: boolean(), Pos :: {X :: integer(), Y :: integer()}},
                             This :: wxListCtrl(),
                             Item :: integer().
```

Returns the position of the item, in icon or small icon view.

# `getItemRect`

```erlang
-spec getItemRect(This, Item) -> Result
                     when
                         Result ::
                             {Res :: boolean(),
                              Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}},
                         This :: wxListCtrl(),
                         Item :: integer().
```

# `getItemRect`

```erlang
-spec getItemRect(This, Item, [Option]) -> Result
                     when
                         Result ::
                             {Res :: boolean(),
                              Rect :: {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}},
                         This :: wxListCtrl(),
                         Item :: integer(),
                         Option :: {code, integer()}.
```

Returns the rectangle representing the item's size and position, in physical coordinates.

`code` is one of wxLIST_RECT_BOUNDS, wxLIST_RECT_ICON, wxLIST_RECT_LABEL.

# `getItemSpacing`

```erlang
-spec getItemSpacing(This) -> {W :: integer(), H :: integer()} when This :: wxListCtrl().
```

Retrieves the spacing between icons in pixels: horizontal spacing is returned as `x`
component of the {Width,Height} object and the vertical spacing as its `y` component.

# `getItemState`

```erlang
-spec getItemState(This, Item, StateMask) -> integer()
                      when This :: wxListCtrl(), Item :: integer(), StateMask :: integer().
```

Gets the item state.

For a list of state flags, see `setItem/5`. The `stateMask` indicates which state flags are of
interest.

# `getItemText`

```erlang
-spec getItemText(This, Item) -> unicode:charlist() when This :: wxListCtrl(), Item :: integer().
```

# `getItemText`

```erlang
-spec getItemText(This, Item, [Option]) -> unicode:charlist()
                     when This :: wxListCtrl(), Item :: integer(), Option :: {col, integer()}.
```

Gets the item text for this item.

# `getItemTextColour`

```erlang
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when This :: wxListCtrl(), Item :: integer().
```

Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the default
foreground control of the control itself as this wouldn't allow distinguishing between
items having the same colour as the current control foreground and items with default
colour which, hence, have always the same colour as the control).

# `getNextItem`

```erlang
-spec getNextItem(This, Item) -> integer() when This :: wxListCtrl(), Item :: integer().
```

# `getNextItem`

```erlang
-spec getNextItem(This, Item, [Option]) -> integer()
                     when
                         This :: wxListCtrl(),
                         Item :: integer(),
                         Option :: {geometry, integer()} | {state, integer()}.
```

Searches for an item with the given geometry or state, starting from `item` but excluding
the `item` itself.

If `item` is -1, the first item that matches the specified flags will be returned.
Returns the first item with given state following `item` or -1 if no such item found. This
function may be used to find all selected items in the control like this:

`geometry` can be one of:

* wxLIST_NEXT_ABOVE: Searches for an item above the specified item.

* wxLIST_NEXT_ALL: Searches for subsequent item by index.

* wxLIST_NEXT_BELOW: Searches for an item below the specified item.

* wxLIST_NEXT_LEFT: Searches for an item to the left of the specified item.

* wxLIST_NEXT_RIGHT: Searches for an item to the right of the specified item.

Note: this parameter is only supported by wxMSW currently and ignored on other platforms.

`state` can be a bitlist of the following:

* wxLIST_STATE_DONTCARE: Don't care what the state is.

* wxLIST_STATE_DROPHILITED: The item indicates it is a drop target.

* wxLIST_STATE_FOCUSED: The item has the focus.

* wxLIST_STATE_SELECTED: The item is selected.

* wxLIST_STATE_CUT: The item is selected as part of a cut and paste operation.

# `getSelectedItemCount`

```erlang
-spec getSelectedItemCount(This) -> integer() when This :: wxListCtrl().
```

Returns the number of selected items in the list control.

# `getTextColour`

```erlang
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxListCtrl().
```

Gets the text colour of the list control.

# `getTopItem`

```erlang
-spec getTopItem(This) -> integer() when This :: wxListCtrl().
```

Gets the index of the topmost visible item when in list or report view.

# `getViewRect`

```erlang
-spec getViewRect(This) -> {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}
                     when This :: wxListCtrl().
```

Returns the rectangle taken by all items in the control.

In other words, if the controls client size were equal to the size of this rectangle, no
scrollbars would be needed and no free space would be left.

Note that this function only works in the icon and small icon views, not in list or
report views (this is a limitation of the native Win32 control).

# `hitTest`

```erlang
-spec hitTest(This, Point) -> Result
                 when
                     Result :: {Res :: integer(), Flags :: integer(), PtrSubItem :: integer()},
                     This :: wxListCtrl(),
                     Point :: {X :: integer(), Y :: integer()}.
```

Determines which item (if any) is at the specified point, giving details in `flags`.

Returns index of the item or `wxNOT_FOUND` if no item is at the specified point.

`flags` will be a combination of the following flags:

* wxLIST_HITTEST_ABOVE: Above the control's client area.

* wxLIST_HITTEST_BELOW: Below the control's client area.

* wxLIST_HITTEST_TOLEFT: To the left of the control's client area.

* wxLIST_HITTEST_TORIGHT: To the right of the control's client area.

* wxLIST_HITTEST_NOWHERE: Inside the control's client area but not over an item.

* wxLIST_HITTEST_ONITEMICON: Over an item's icon.

* wxLIST_HITTEST_ONITEMLABEL: Over an item's text.

* wxLIST_HITTEST_ONITEMSTATEICON: Over the checkbox of an item.

* wxLIST_HITTEST_ONITEM: Combination of `wxLIST_HITTEST_ONITEMICON`, `wxLIST_HITTEST_ONITEMLABEL`, `wxLIST_HITTEST_ONITEMSTATEICON`.

If `ptrSubItem` is not NULL and the `m:wxListCtrl` is in the report mode the subitem (or
column) number will also be provided. This feature is only available in version 2.7.0 or
higher and is currently only implemented under wxMSW and requires at least comctl32.dll of
version 4.70 on the host system or the value stored in `ptrSubItem` will be always -1. To
compile this feature into wxWidgets library you need to have access to commctrl.h of
version 4.70 that is provided by Microsoft.

# `insertColumn`

```erlang
-spec insertColumn(This, Col, Heading) -> integer()
                      when This :: wxListCtrl(), Col :: integer(), Heading :: unicode:chardata();
                  (This, Col, Info) -> integer()
                      when This :: wxListCtrl(), Col :: integer(), Info :: wxListItem:wxListItem().
```

For report view mode (only), inserts a column.

For more details, see `setItem/5`. Also see `insertColumn/4` overload for a usually more convenient alternative to
this method and the description of how the item width is interpreted by this method.

# `insertColumn`

```erlang
-spec insertColumn(This, Col, Heading, [Option]) -> integer()
                      when
                          This :: wxListCtrl(),
                          Col :: integer(),
                          Heading :: unicode:chardata(),
                          Option :: {format, integer()} | {width, integer()}.
```

For report view mode (only), inserts a column.

Insert a new column in the list control in report view mode at the given position
specifying its most common attributes.

Notice that to set the image for the column you need to use `insertColumn/4` overload and specify
?wxLIST\_MASK\_IMAGE in the item mask.

Return: The index of the inserted column or -1 if adding it failed.

# `insertItem`

```erlang
-spec insertItem(This, Info) -> integer() when This :: wxListCtrl(), Info :: wxListItem:wxListItem().
```

Inserts an item, returning the index of the new item if successful, -1 otherwise.

# `insertItem`

```erlang
-spec insertItem(This, Index, ImageIndex) -> integer()
                    when This :: wxListCtrl(), Index :: integer(), ImageIndex :: integer();
                (This, Index, Label) -> integer()
                    when This :: wxListCtrl(), Index :: integer(), Label :: unicode:chardata().
```

Insert a string item.

# `insertItem`

```erlang
-spec insertItem(This, Index, Label, ImageIndex) -> integer()
                    when
                        This :: wxListCtrl(),
                        Index :: integer(),
                        Label :: unicode:chardata(),
                        ImageIndex :: integer().
```

Insert an image/string item.

# `new`

```erlang
-spec new() -> wxListCtrl().
```

# `new`

```erlang
-spec new(Parent) -> wxListCtrl() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxListCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {winid, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()} |
                     {onGetItemText, function()} |
                     {onGetItemAttr, function()} |
                     {onGetItemColumnImage, function()}.
```

# `refreshItem`

```erlang
-spec refreshItem(This, Item) -> ok when This :: wxListCtrl(), Item :: integer().
```

Redraws the given `item`.

This is only useful for the virtual list controls as without calling this function the
displayed value of the item doesn't change even when the underlying data does change.

See: `refreshItems/3`

# `refreshItems`

```erlang
-spec refreshItems(This, ItemFrom, ItemTo) -> ok
                      when This :: wxListCtrl(), ItemFrom :: integer(), ItemTo :: integer().
```

Redraws the items between `itemFrom` and `itemTo`.

The starting item must be less than or equal to the ending one.

Just as `refreshItem/2` this is only useful for virtual list controls.

# `scrollList`

```erlang
-spec scrollList(This, Dx, Dy) -> boolean() when This :: wxListCtrl(), Dx :: integer(), Dy :: integer().
```

Scrolls the list control.

If in icon, small icon or report view mode, `dx` specifies the number of pixels to
scroll. If in list view mode, `dx` specifies the number of columns to scroll. `dy` always
specifies the number of pixels to scroll vertically.

Note: This method is currently only implemented in the Windows version.

# `setBackgroundColour`

```erlang
-spec setBackgroundColour(This, Col) -> boolean() when This :: wxListCtrl(), Col :: wx:wx_colour().
```

Sets the background colour.

Note that the `wxWindow:getBackgroundColour/1` function of `m:wxWindow` base class can be used to retrieve the current
background colour.

# `setColumn`

```erlang
-spec setColumn(This, Col, Item) -> boolean()
                   when This :: wxListCtrl(), Col :: integer(), Item :: wxListItem:wxListItem().
```

Sets information about this column.

See `setItem/5` for more information.

# `setColumnWidth`

```erlang
-spec setColumnWidth(This, Col, Width) -> boolean()
                        when This :: wxListCtrl(), Col :: integer(), Width :: integer().
```

Sets the column width.

`width` can be a width in pixels or `wxLIST_AUTOSIZE` (-1) or `wxLIST_AUTOSIZE_USEHEADER` (-2).

`wxLIST_AUTOSIZE` will resize the column to the length of its longest item.

`wxLIST_AUTOSIZE_USEHEADER` will resize the column to the length of the header (Win32) or
80 pixels (other platforms).

In small or normal icon view, `col` must be -1, and the column width is set for all
columns.

# `setImageList`

```erlang
-spec setImageList(This, ImageList, Which) -> ok
                      when
                          This :: wxListCtrl(),
                          ImageList :: wxImageList:wxImageList(),
                          Which :: integer().
```

Sets the image list associated with the control.

`which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`, `wxIMAGE_LIST_STATE` (the
last is unimplemented).

This method does not take ownership of the image list, you have to delete it yourself.

See: `assignImageList/3`

# `setItem`

```erlang
-spec setItem(This, Info) -> boolean() when This :: wxListCtrl(), Info :: wxListItem:wxListItem().
```

Sets the data of an item.

Using the `m:wxListItem`'s mask and state mask, you can change only selected attributes
of a `m:wxListCtrl` item.

Return: true if the item was successfully updated or false if the update failed for some
reason (e.g. an invalid item index).

# `setItem`

```erlang
-spec setItem(This, Index, Column, Label) -> boolean()
                 when
                     This :: wxListCtrl(),
                     Index :: integer(),
                     Column :: integer(),
                     Label :: unicode:chardata().
```

# `setItem`

```erlang
-spec setItem(This, Index, Column, Label, [Option]) -> boolean()
                 when
                     This :: wxListCtrl(),
                     Index :: integer(),
                     Column :: integer(),
                     Label :: unicode:chardata(),
                     Option :: {imageId, integer()}.
```

Sets an item string field at a particular column.

Return: true if the item was successfully updated or false if the update failed for some
reason (e.g. an invalid item index).

# `setItemBackgroundColour`

```erlang
-spec setItemBackgroundColour(This, Item, Col) -> ok
                                 when This :: wxListCtrl(), Item :: integer(), Col :: wx:wx_colour().
```

Sets the background colour for this item.

This function only works in report view mode. The colour can be retrieved using `getItemBackgroundColour/2`.

# `setItemColumnImage`

```erlang
-spec setItemColumnImage(This, Item, Column, Image) -> boolean()
                            when
                                This :: wxListCtrl(),
                                Item :: integer(),
                                Column :: integer(),
                                Image :: integer().
```

Sets the image associated with the item.

In report view, you can specify the column. The image is an index into the image list
associated with the list control.

# `setItemCount`

```erlang
-spec setItemCount(This, Count) -> ok when This :: wxListCtrl(), Count :: integer().
```

This method can only be used with virtual list controls.

It is used to indicate to the control the number of items it contains. After calling it,
the main program should be ready to handle calls to various item callbacks (such as `wxListCtrl::OnGetItemText`
(not implemented in wx)) for all items in the range from 0 to `count`.

Notice that the control is not necessarily redrawn after this call as it may be
undesirable if an item which is not visible on the screen anyhow was added to or removed
from a control displaying many items, if you do need to refresh the display you can just
call `wxWindow:refresh/2` manually.

# `setItemData`

```erlang
-spec setItemData(This, Item, Data) -> boolean()
                     when This :: wxListCtrl(), Item :: integer(), Data :: integer().
```

Associates application-defined data with this item.

Notice that this function cannot be used to associate pointers with the control items,
use `SetItemPtrData()` (not implemented in wx) instead.

# `setItemFont`

```erlang
-spec setItemFont(This, Item, Font) -> ok
                     when This :: wxListCtrl(), Item :: integer(), Font :: wxFont:wxFont().
```

Sets the item's font.

# `setItemImage`

```erlang
-spec setItemImage(This, Item, Image) -> boolean()
                      when This :: wxListCtrl(), Item :: integer(), Image :: integer().
```

# `setItemImage`

```erlang
-spec setItemImage(This, Item, Image, [Option]) -> boolean()
                      when
                          This :: wxListCtrl(),
                          Item :: integer(),
                          Image :: integer(),
                          Option :: {selImage, integer()}.
```

Sets the unselected and selected images associated with the item.

The images are indices into the image list associated with the list control.

# `setItemPosition`

```erlang
-spec setItemPosition(This, Item, Pos) -> boolean()
                         when
                             This :: wxListCtrl(),
                             Item :: integer(),
                             Pos :: {X :: integer(), Y :: integer()}.
```

Sets the position of the item, in icon or small icon view.

Windows only.

# `setItemState`

```erlang
-spec setItemState(This, Item, State, StateMask) -> boolean()
                      when
                          This :: wxListCtrl(),
                          Item :: integer(),
                          State :: integer(),
                          StateMask :: integer().
```

Sets the item state.

The `stateMask` is a combination of `wxLIST_STATE_XXX` constants described in `m:wxListItem`
documentation. For each of the bits specified in `stateMask`, the corresponding state is
set or cleared depending on whether `state` argument contains the same bit or not.

So to select an item you can use while to deselect it you should use

Consider using `m:wxListView` if possible to avoid dealing with this error-prone and
confusing method.

Also notice that contrary to the usual rule that only user actions generate events, this
method does generate wxEVT_LIST_ITEM_SELECTED event when it is used to select an item.

# `setItemText`

```erlang
-spec setItemText(This, Item, Text) -> ok
                     when This :: wxListCtrl(), Item :: integer(), Text :: unicode:chardata().
```

Sets the item text for this item.

# `setItemTextColour`

```erlang
-spec setItemTextColour(This, Item, Col) -> ok
                           when This :: wxListCtrl(), Item :: integer(), Col :: wx:wx_colour().
```

Sets the colour for this item.

This function only works in report view. The colour can be retrieved using `getItemTextColour/2`.

# `setSingleStyle`

```erlang
-spec setSingleStyle(This, Style) -> ok when This :: wxListCtrl(), Style :: integer().
```

# `setSingleStyle`

```erlang
-spec setSingleStyle(This, Style, [Option]) -> ok
                        when This :: wxListCtrl(), Style :: integer(), Option :: {add, boolean()}.
```

Adds or removes a single window style.

# `setTextColour`

```erlang
-spec setTextColour(This, Col) -> ok when This :: wxListCtrl(), Col :: wx:wx_colour().
```

Sets the text colour of the list control.

# `setWindowStyleFlag`

```erlang
-spec setWindowStyleFlag(This, Style) -> ok when This :: wxListCtrl(), Style :: integer().
```

Sets the whole window style, deleting all items.

# `sortItems`

```erlang
-spec sortItems(This :: wxListCtrl(), SortCallBack) -> boolean()
                   when SortCallBack :: fun((integer(), integer()) -> integer()).
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
