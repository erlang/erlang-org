# `wxTreeCtrl`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxTreeCtrl.erl#L62)

A tree control presents information as a hierarchy, with items that may be expanded to
show further items.

Items in a tree control are referenced by `wxTreeItemId` (not implemented in wx) handles,
which may be tested for validity by calling `wxTreeItemId::IsOk()` (not implemented in wx).

A similar control with a fully native implementation for GTK+ and macOS as well is `wxDataViewTreeCtrl`
(not implemented in wx).

To intercept events from a tree control, use the event table macros described in `m:wxTreeEvent`.

## Styles

This class supports the following styles:

* wxTR_EDIT_LABELS: Use this style if you wish the user to be able to edit labels in the
tree control.

* wxTR_NO_BUTTONS: For convenience to document that no buttons are to be drawn.

* wxTR_HAS_BUTTONS: Use this style to show + and - buttons to the left of parent items.

* wxTR_TWIST_BUTTONS: Selects alternative style of +/`-` buttons and shows rotating
("twisting") arrows instead. Currently this style is only implemented under Microsoft
Windows Vista and later Windows versions and is ignored under the other platforms as
enabling it is equivalent to using `wxSystemThemedControl::EnableSystemTheme()` (not
implemented in wx).

* wxTR_NO_LINES: Use this style to hide vertical level connectors.

* wxTR_FULL_ROW_HIGHLIGHT: Use this style to have the background colour and the selection
highlight extend over the entire horizontal row of the tree control window. (This flag is
ignored under Windows unless you specify `wxTR_NO_LINES` as well.)

* wxTR_LINES_AT_ROOT: Use this style to show lines leading to the root nodes (unless no `wxTR_NO_LINES`
is also used, in which case no lines are shown). Note that in the MSW version, if this
style is omitted, not only the lines, but also the button used for expanding the root item
is not shown, which can be unexpected, so it is recommended to always use it.

* wxTR_HIDE_ROOT: Use this style to suppress the display of the root node, effectively
causing the first-level nodes to appear as a series of root nodes.

* wxTR_ROW_LINES: Use this style to draw a contrasting border between displayed rows.

* wxTR_HAS_VARIABLE_ROW_HEIGHT: Use this style to cause row heights to be just big enough
to fit the content. If not set, all rows use the largest row height. The default is that
this flag is unset. Generic only.

* wxTR_SINGLE: For convenience to document that only one item may be selected at a time.
Selecting another item causes the current selection, if any, to be deselected. This is the
default.

* wxTR_MULTIPLE: Use this style to allow a range of items to be selected. If a second range
is selected, the current range, if any, is deselected.

* wxTR_DEFAULT_STYLE: The set of flags that are closest to the defaults for the native
control for a particular toolkit.

See also overview_windowstyles.

`Win32` `notes:`

`m:wxTreeCtrl` class uses the standard common treeview control under Win32 implemented in
the system library comctl32.dll. Some versions of this library are known to have bugs with
handling the tree control colours: the usual symptom is that the expanded items leave
black (or otherwise incorrectly coloured) background behind them, especially for the
controls using non-default background colour. The recommended solution is to upgrade the
comctl32.dll to a newer version: see [http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2](http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2)

See:
* `m:wxTreeEvent`

* [Overview treectrl](https://docs.wxwidgets.org/3.2/overview_treectrl.html#overview_treectrl)

* `m:wxListBox`

* `m:wxListCtrl`

* `m:wxImageList`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTreeCtrl](https://docs.wxwidgets.org/3.2/classwx_tree_ctrl.html)

## Events

Event types emitted from this class:

* [`command_tree_begin_drag`](`m:wxTreeEvent`)

* [`command_tree_begin_rdrag`](`m:wxTreeEvent`)

* [`command_tree_end_drag`](`m:wxTreeEvent`)

* [`command_tree_begin_label_edit`](`m:wxTreeEvent`)

* [`command_tree_end_label_edit`](`m:wxTreeEvent`)

* [`command_tree_delete_item`](`m:wxTreeEvent`)

* [`command_tree_get_info`](`m:wxTreeEvent`)

* [`command_tree_set_info`](`m:wxTreeEvent`)

* [`command_tree_item_activated`](`m:wxTreeEvent`)

* [`command_tree_item_collapsed`](`m:wxTreeEvent`)

* [`command_tree_item_collapsing`](`m:wxTreeEvent`)

* [`command_tree_item_expanded`](`m:wxTreeEvent`)

* [`command_tree_item_expanding`](`m:wxTreeEvent`)

* [`command_tree_item_right_click`](`m:wxTreeEvent`)

* [`command_tree_item_middle_click`](`m:wxTreeEvent`)

* [`command_tree_sel_changed`](`m:wxTreeEvent`)

* [`command_tree_sel_changing`](`m:wxTreeEvent`)

* [`command_tree_key_down`](`m:wxTreeEvent`)

* [`command_tree_item_gettooltip`](`m:wxTreeEvent`)

* [`command_tree_item_menu`](`m:wxTreeEvent`)

* [`command_tree_state_image_click`](`m:wxTreeEvent`)

# `wxTreeCtrl`

```erlang
-type wxTreeCtrl() :: wx:wx_object().
```

# `addRoot`

```erlang
-spec addRoot(This, Text) -> integer() when This :: wxTreeCtrl(), Text :: unicode:chardata().
```

# `addRoot`

```erlang
-spec addRoot(This, Text, [Option]) -> integer()
                 when
                     This :: wxTreeCtrl(),
                     Text :: unicode:chardata(),
                     Option :: {image, integer()} | {selectedImage, integer()} | {data, term()}.
```

Adds the root node to the tree, returning the new item.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.

# `appendItem`

```erlang
-spec appendItem(This, Parent, Text) -> integer()
                    when This :: wxTreeCtrl(), Parent :: integer(), Text :: unicode:chardata().
```

# `appendItem`

```erlang
-spec appendItem(This, Parent, Text, [Option]) -> integer()
                    when
                        This :: wxTreeCtrl(),
                        Parent :: integer(),
                        Text :: unicode:chardata(),
                        Option :: {image, integer()} | {selectedImage, integer()} | {data, term()}.
```

Appends an item to the end of the branch identified by `parent`, return a new item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.

# `assignImageList`

```erlang
-spec assignImageList(This, ImageList) -> ok
                         when This :: wxTreeCtrl(), ImageList :: wxImageList:wxImageList().
```

Sets the normal image list.

The image list assigned with this method will be automatically deleted by `m:wxTreeCtrl`
as appropriate (i.e. it takes ownership of the list).

See: `setImageList/2`

# `assignStateImageList`

```erlang
-spec assignStateImageList(This, ImageList) -> ok
                              when This :: wxTreeCtrl(), ImageList :: wxImageList:wxImageList().
```

Sets the state image list.

Image list assigned with this method will be automatically deleted by `m:wxTreeCtrl` as
appropriate (i.e. it takes ownership of the list).

See: `setStateImageList/2`

# `collapse`

```erlang
-spec collapse(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Collapses the given item.

# `collapseAndReset`

```erlang
-spec collapseAndReset(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Collapses the given item and removes all children.

# `create`

```erlang
-spec create(This, Parent) -> boolean() when This :: wxTreeCtrl(), Parent :: wxWindow:wxWindow().
```

# `create`

```erlang
-spec create(This, Parent, [Option]) -> boolean()
                when
                    This :: wxTreeCtrl(),
                    Parent :: wxWindow:wxWindow(),
                    Option ::
                        {id, integer()} |
                        {pos, {X :: integer(), Y :: integer()}} |
                        {size, {W :: integer(), H :: integer()}} |
                        {style, integer()} |
                        {validator, wx:wx_object()}.
```

Creates the tree control.

See `new/2` for further details.

# `delete`

```erlang
-spec delete(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Deletes the specified item.

A `EVT_TREE_DELETE_ITEM` event will be generated.

This function may cause a subsequent call to `getNextChild/3` to fail.

# `deleteAllItems`

```erlang
-spec deleteAllItems(This) -> ok when This :: wxTreeCtrl().
```

Deletes all items in the control.

This function generates `wxEVT_TREE_DELETE_ITEM` events for each item being deleted,
including the root one if it is shown, i.e. unless wxTR_HIDE_ROOT style is used.

# `deleteChildren`

```erlang
-spec deleteChildren(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Deletes all children of the given item (but not the item itself).

A `wxEVT_TREE_DELETE_ITEM` event will be generated for every item being deleted.

If you have called `setItemHasChildren/3`, you may need to call it again since `deleteChildren/2` does not automatically clear
the setting.

# `destroy`

```erlang
-spec destroy(This :: wxTreeCtrl()) -> ok.
```

Destroys the object

# `editLabel`

```erlang
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when This :: wxTreeCtrl(), Item :: integer().
```

Starts editing the label of the given `item`.

This function generates a `EVT_TREE_BEGIN_LABEL_EDIT` event which can be vetoed so that
no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text control
without changes, a `EVT_TREE_END_LABEL_EDIT` event will be sent which can be vetoed as well.

See: `m:wxTreeEvent`

# `ensureVisible`

```erlang
-spec ensureVisible(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Scrolls and/or expands items to ensure that the given item is visible.

This method can be used, and will work, even while the window is frozen (see `wxWindow:freeze/1`).

# `expand`

```erlang
-spec expand(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Expands the given item.

# `getBoundingRect`

```erlang
-spec getBoundingRect(This, Item) -> Result
                         when
                             Result ::
                                 {Res :: boolean(),
                                  Rect ::
                                      {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}},
                             This :: wxTreeCtrl(),
                             Item :: integer().
```

# `getBoundingRect`

```erlang
-spec getBoundingRect(This, Item, [Option]) -> Result
                         when
                             Result ::
                                 {Res :: boolean(),
                                  Rect ::
                                      {X :: integer(), Y :: integer(), W :: integer(), H :: integer()}},
                             This :: wxTreeCtrl(),
                             Item :: integer(),
                             Option :: {textOnly, boolean()}.
```

Retrieves the rectangle bounding the `item`.

If `textOnly` is true, only the rectangle around the item's label will be returned,
otherwise the item's image is also taken into account.

The return value is true if the rectangle was successfully retrieved or false if it was
not (in this case `rect` is not changed) - for example, if the item is currently invisible.

Notice that the rectangle coordinates are logical, not physical ones. So, for example,
the x coordinate may be negative if the tree has a horizontal scrollbar and its position
is not 0.

# `getChildrenCount`

```erlang
-spec getChildrenCount(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

# `getChildrenCount`

```erlang
-spec getChildrenCount(This, Item, [Option]) -> integer()
                          when
                              This :: wxTreeCtrl(),
                              Item :: integer(),
                              Option :: {recursively, boolean()}.
```

Returns the number of items in the branch.

If `recursively` is true, returns the total number of descendants, otherwise only one
level of children is counted.

# `getCount`

```erlang
-spec getCount(This) -> integer() when This :: wxTreeCtrl().
```

Returns the number of items in the control.

# `getEditControl`

```erlang
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when This :: wxTreeCtrl().
```

Returns the edit control being currently used to edit a label.

Returns NULL if no label is being edited.

Note: This is currently only implemented for wxMSW.

# `getFirstChild`

```erlang
-spec getFirstChild(This, Item) -> Result
                       when
                           Result :: {Res :: integer(), Cookie :: integer()},
                           This :: wxTreeCtrl(),
                           Item :: integer().
```

Returns the first child; call `getNextChild/3` for the next child.

For this enumeration function you must pass in a 'cookie' parameter which is opaque for
the application but is necessary for the library to make these functions reentrant (i.e.
allow more than one enumeration on one and the same object simultaneously). The cookie
passed to `getFirstChild/2` and `getNextChild/3` should be the same variable.

Returns an invalid tree item (i.e. `wxTreeItemId::IsOk()` (not implemented in wx) returns
false) if there are no further children.

See:
* `getNextChild/3`

* `getNextSibling/2`

# `getFirstVisibleItem`

```erlang
-spec getFirstVisibleItem(This) -> integer() when This :: wxTreeCtrl().
```

Returns the first visible item.

# `getImageList`

```erlang
-spec getImageList(This) -> wxImageList:wxImageList() when This :: wxTreeCtrl().
```

Returns the normal image list.

# `getIndent`

```erlang
-spec getIndent(This) -> integer() when This :: wxTreeCtrl().
```

Returns the current tree control indentation.

# `getItemBackgroundColour`

```erlang
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4()
                                 when This :: wxTreeCtrl(), Item :: integer().
```

Returns the background colour of the item.

# `getItemData`

```erlang
-spec getItemData(This, Item) -> term() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the tree item data associated with the item.

# `getItemFont`

```erlang
-spec getItemFont(This, Item) -> wxFont:wxFont() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the font of the item label.

If the font hadn't been explicitly set for the specified `item` with `setItemFont/3`, returns an invalid
?wxNullFont font. `wxWindow:getFont/1` can be used to retrieve the global tree control font used for the items
without any specific font.

# `getItemImage`

```erlang
-spec getItemImage(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

# `getItemImage`

```erlang
-spec getItemImage(This, Item, [Option]) -> integer()
                      when This :: wxTreeCtrl(), Item :: integer(), Option :: {which, wx:wx_enum()}.
```

Gets the specified item image.

The value of `which` may be:

* ?wxTreeItemIcon\_Normal: to get the normal item image.

* ?wxTreeItemIcon\_Selected: to get the selected item image (i.e. the image which is shown
when the item is currently selected).

* ?wxTreeItemIcon\_Expanded: to get the expanded image (this only makes sense for items
which have children - then this image is shown when the item is expanded and the normal
image is shown when it is collapsed).

* ?wxTreeItemIcon\_SelectedExpanded: to get the selected expanded image (which is shown
when an expanded item is currently selected).

# `getItemParent`

```erlang
-spec getItemParent(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the item's parent.

# `getItemText`

```erlang
-spec getItemText(This, Item) -> unicode:charlist() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the item label.

# `getItemTextColour`

```erlang
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the colour of the item label.

# `getLastChild`

```erlang
-spec getLastChild(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the last child of the item (or an invalid tree item if this item has no
children).

See:
* `getFirstChild/2`

* `getNextSibling/2`

* `getLastChild/2`

# `getNextChild`

```erlang
-spec getNextChild(This, Item, Cookie) -> Result
                      when
                          Result :: {Res :: integer(), Cookie :: integer()},
                          This :: wxTreeCtrl(),
                          Item :: integer(),
                          Cookie :: integer().
```

Returns the next child; call `getFirstChild/2` for the first child.

For this enumeration function you must pass in a 'cookie' parameter which is opaque for
the application but is necessary for the library to make these functions reentrant (i.e.
allow more than one enumeration on one and the same object simultaneously). The cookie
passed to `getFirstChild/2` and `getNextChild/3` should be the same.

Returns an invalid tree item if there are no further children.

See: `getFirstChild/2`

# `getNextSibling`

```erlang
-spec getNextSibling(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the next sibling of the specified item; call `getPrevSibling/2` for the previous
sibling.

Returns an invalid tree item if there are no further siblings.

See: `getPrevSibling/2`

# `getNextVisible`

```erlang
-spec getNextVisible(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the next visible item or an invalid item if this item is the last visible one.

Note: The `item` itself must be visible.

# `getPrevSibling`

```erlang
-spec getPrevSibling(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the previous sibling of the specified item; call `getNextSibling/2` for the next
sibling.

Returns an invalid tree item if there are no further children.

See: `getNextSibling/2`

# `getPrevVisible`

```erlang
-spec getPrevVisible(This, Item) -> integer() when This :: wxTreeCtrl(), Item :: integer().
```

Returns the previous visible item or an invalid item if this item is the first visible
one.

Note: The `item` itself must be visible.

# `getRootItem`

```erlang
-spec getRootItem(This) -> integer() when This :: wxTreeCtrl().
```

Returns the root item for the tree control.

# `getSelection`

```erlang
-spec getSelection(This) -> integer() when This :: wxTreeCtrl().
```

Returns the selection, or an invalid item if there is no selection.

This function only works with the controls without `wxTR_MULTIPLE` style, use `getSelections/1` for the
controls which do have this style or, if a single item is wanted, use `GetFocusedItem()`
(not implemented in wx).

# `getSelections`

```erlang
-spec getSelections(This) -> Result
                       when Result :: {Res :: integer(), Selection :: [integer()]}, This :: wxTreeCtrl().
```

Fills the array of tree items passed in with the currently selected items.

This function can be called only if the control has the `wxTR_MULTIPLE` style.

Returns the number of selected items.

# `getStateImageList`

```erlang
-spec getStateImageList(This) -> wxImageList:wxImageList() when This :: wxTreeCtrl().
```

Returns the state image list (from which application-defined state images are taken).

# `hitTest`

```erlang
-spec hitTest(This, Point) -> Result
                 when
                     Result :: {Res :: integer(), Flags :: integer()},
                     This :: wxTreeCtrl(),
                     Point :: {X :: integer(), Y :: integer()}.
```

Calculates which (if any) item is under the given `point`, returning the tree item id at
this point plus extra information `flags`.

`flags` is a bitlist of the following:

* `wxTREE_HITTEST_ABOVE:` Above the client area.

* `wxTREE_HITTEST_BELOW:` Below the client area.

* `wxTREE_HITTEST_NOWHERE:` In the client area but below the last item.

* `wxTREE_HITTEST_ONITEMBUTTON:` On the button associated with an item.

* `wxTREE_HITTEST_ONITEMICON:` On the bitmap associated with an item.

* `wxTREE_HITTEST_ONITEMINDENT:` In the indentation associated with an item.

* `wxTREE_HITTEST_ONITEMLABEL:` On the label (string) associated with an item.

* `wxTREE_HITTEST_ONITEMRIGHT:` In the area to the right of an item.

* `wxTREE_HITTEST_ONITEMSTATEICON:` On the state icon for a tree view item that is in a
user-defined state.

* `wxTREE_HITTEST_TOLEFT:` To the right of the client area.

* `wxTREE_HITTEST_TORIGHT:` To the left of the client area.

# `insertItem`

```erlang
-spec insertItem(This, Parent, Previous, Text) -> integer()
                    when
                        This :: wxTreeCtrl(),
                        Parent :: integer(),
                        Previous :: integer(),
                        Text :: unicode:chardata().
```

# `insertItem`

```erlang
-spec insertItem(This, Parent, Previous, Text, [Option]) -> integer()
                    when
                        This :: wxTreeCtrl(),
                        Parent :: integer(),
                        Previous :: integer(),
                        Text :: unicode:chardata(),
                        Option :: {image, integer()} | {selImage, integer()} | {data, term()}.
```

Inserts an item after a given one (`previous`).

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.

# `isBold`

```erlang
-spec isBold(This, Item) -> boolean() when This :: wxTreeCtrl(), Item :: integer().
```

Returns true if the given item is in bold state.

See: `setItemBold/3`

# `isExpanded`

```erlang
-spec isExpanded(This, Item) -> boolean() when This :: wxTreeCtrl(), Item :: integer().
```

Returns true if the item is expanded (only makes sense if it has children).

# `isSelected`

```erlang
-spec isSelected(This, Item) -> boolean() when This :: wxTreeCtrl(), Item :: integer().
```

Returns true if the item is selected.

# `isTreeItemIdOk`

```erlang
-spec isTreeItemIdOk(Item) -> boolean() when Item :: integer().
```

Returns true if the item is valid.

# `isVisible`

```erlang
-spec isVisible(This, Item) -> boolean() when This :: wxTreeCtrl(), Item :: integer().
```

Returns true if the item is visible on the screen.

# `itemHasChildren`

```erlang
-spec itemHasChildren(This, Item) -> boolean() when This :: wxTreeCtrl(), Item :: integer().
```

Returns true if the item has children.

# `new`

```erlang
-spec new() -> wxTreeCtrl().
```

Default Constructor.

# `new`

```erlang
-spec new(Parent) -> wxTreeCtrl() when Parent :: wxWindow:wxWindow().
```

# `new`

```erlang
-spec new(Parent, [Option]) -> wxTreeCtrl()
             when
                 Parent :: wxWindow:wxWindow(),
                 Option ::
                     {id, integer()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a tree control.

See: `create/3`

# `prependItem`

```erlang
-spec prependItem(This, Parent, Text) -> integer()
                     when This :: wxTreeCtrl(), Parent :: integer(), Text :: unicode:chardata().
```

# `prependItem`

```erlang
-spec prependItem(This, Parent, Text, [Option]) -> integer()
                     when
                         This :: wxTreeCtrl(),
                         Parent :: integer(),
                         Text :: unicode:chardata(),
                         Option :: {image, integer()} | {selectedImage, integer()} | {data, term()}.
```

Appends an item as the first child of `parent`, return a new item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.

# `scrollTo`

```erlang
-spec scrollTo(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Scrolls the specified item into view.

Note that this method doesn't work while the window is frozen (See `wxWindow:freeze/1`), at least under MSW.

See: `ensureVisible/2`

# `selectItem`

```erlang
-spec selectItem(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

# `selectItem`

```erlang
-spec selectItem(This, Item, [Option]) -> ok
                    when This :: wxTreeCtrl(), Item :: integer(), Option :: {select, boolean()}.
```

Selects the given item.

In multiple selection controls, can be also used to deselect a currently selected item if
the value of `select` is false.

Notice that calling this method will generate `wxEVT_TREE_SEL_CHANGING` and `wxEVT_TREE_SEL_CHANGED`
events and that the change could be vetoed by the former event handler.

# `setImageList`

```erlang
-spec setImageList(This, ImageList) -> ok
                      when This :: wxTreeCtrl(), ImageList :: wxImageList:wxImageList().
```

Sets the normal image list.

The image list assigned with this method will `not` be deleted by `m:wxTreeCtrl`'s
destructor, you must delete it yourself.

See: `assignImageList/2`

# `setIndent`

```erlang
-spec setIndent(This, Indent) -> ok when This :: wxTreeCtrl(), Indent :: integer().
```

Sets the indentation for the tree control.

# `setItemBackgroundColour`

```erlang
-spec setItemBackgroundColour(This, Item, Col) -> ok
                                 when This :: wxTreeCtrl(), Item :: integer(), Col :: wx:wx_colour().
```

Sets the colour of the item's background.

# `setItemBold`

```erlang
-spec setItemBold(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

# `setItemBold`

```erlang
-spec setItemBold(This, Item, [Option]) -> ok
                     when This :: wxTreeCtrl(), Item :: integer(), Option :: {bold, boolean()}.
```

Makes item appear in bold font if `bold` parameter is true or resets it to the normal
state.

See: `isBold/2`

# `setItemData`

```erlang
-spec setItemData(This, Item, Data) -> ok when This :: wxTreeCtrl(), Item :: integer(), Data :: term().
```

Sets the item client data.

Notice that the client data previously associated with the `item` (if any) is `not` freed
by this function and so calling this function multiple times for the same item will result
in memory leaks unless you delete the old item data pointer yourself.

# `setItemDropHighlight`

```erlang
-spec setItemDropHighlight(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

# `setItemDropHighlight`

```erlang
-spec setItemDropHighlight(This, Item, [Option]) -> ok
                              when
                                  This :: wxTreeCtrl(),
                                  Item :: integer(),
                                  Option :: {highlight, boolean()}.
```

Gives the item the visual feedback for Drag'n'Drop actions, which is useful if something
is dragged from the outside onto the tree control (as opposed to a DnD operation within
the tree control, which already is implemented internally).

# `setItemFont`

```erlang
-spec setItemFont(This, Item, Font) -> ok
                     when This :: wxTreeCtrl(), Item :: integer(), Font :: wxFont:wxFont().
```

Sets the item's font.

All items in the tree should have the same height to avoid text clipping, so the fonts
height should be the same for all of them, although font attributes may vary.

See: `setItemBold/3`

# `setItemHasChildren`

```erlang
-spec setItemHasChildren(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

# `setItemHasChildren`

```erlang
-spec setItemHasChildren(This, Item, [Option]) -> ok
                            when This :: wxTreeCtrl(), Item :: integer(), Option :: {has, boolean()}.
```

Force appearance of the button next to the item.

This is useful to allow the user to expand the items which don't have any children now,
but instead adding them only when needed, thus minimizing memory usage and loading time.

# `setItemImage`

```erlang
-spec setItemImage(This, Item, Image) -> ok
                      when This :: wxTreeCtrl(), Item :: integer(), Image :: integer().
```

# `setItemImage`

```erlang
-spec setItemImage(This, Item, Image, [Option]) -> ok
                      when
                          This :: wxTreeCtrl(),
                          Item :: integer(),
                          Image :: integer(),
                          Option :: {which, wx:wx_enum()}.
```

Sets the specified item's image.

See `getItemImage/3` for the description of the `which` parameter.

# `setItemText`

```erlang
-spec setItemText(This, Item, Text) -> ok
                     when This :: wxTreeCtrl(), Item :: integer(), Text :: unicode:chardata().
```

Sets the item label.

# `setItemTextColour`

```erlang
-spec setItemTextColour(This, Item, Col) -> ok
                           when This :: wxTreeCtrl(), Item :: integer(), Col :: wx:wx_colour().
```

Sets the colour of the item's text.

# `setStateImageList`

```erlang
-spec setStateImageList(This, ImageList) -> ok
                           when This :: wxTreeCtrl(), ImageList :: wxImageList:wxImageList().
```

Sets the state image list (from which application-defined state images are taken).

Image list assigned with this method will `not` be deleted by `m:wxTreeCtrl`'s
destructor, you must delete it yourself.

See: `assignStateImageList/2`

# `setWindowStyle`

```erlang
-spec setWindowStyle(This, Styles) -> ok when This :: wxTreeCtrl(), Styles :: integer().
```

Sets the mode flags associated with the display of the tree control.

The new mode takes effect immediately.

Note: Generic only; MSW ignores changes.

# `sortChildren`

```erlang
-spec sortChildren(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Sorts the children of the given item using `OnCompareItems()` (not implemented in wx).

You should override that method to change the sort order (the default is ascending
case-sensitive alphabetical order).

# `toggle`

```erlang
-spec toggle(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Toggles the given item between collapsed and expanded states.

# `toggleItemSelection`

```erlang
-spec toggleItemSelection(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Toggles the given item between selected and unselected states.

For multiselection controls only.

# `unselect`

```erlang
-spec unselect(This) -> ok when This :: wxTreeCtrl().
```

Removes the selection from the currently selected item (if any).

# `unselectAll`

```erlang
-spec unselectAll(This) -> ok when This :: wxTreeCtrl().
```

This function either behaves the same as `unselect/1` if the control doesn't have `wxTR\_MULTIPLE`
style, or removes the selection from all items if it does have this style.

# `unselectItem`

```erlang
-spec unselectItem(This, Item) -> ok when This :: wxTreeCtrl(), Item :: integer().
```

Unselects the given item.

This works in multiselection controls only.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
