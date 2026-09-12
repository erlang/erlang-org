# `wxListItem`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxListItem.erl#L58)

This class stores information about a `m:wxListCtrl` item or column.

`m:wxListItem` is a class which contains information about:

* Zero based item position; see `setId/2` and `getId/1`.

* Zero based column index; see `setColumn/2` and `getColumn/1`.

* The label (or header for columns); see `setText/2` and `getText/1`.

* The zero based index into an image list; see `getImage/1` and `setImage/2`.

* Application defined data; see `SetData()` (not implemented in wx) and `GetData()` (not
implemented in wx).

* For columns only: the width of the column; see `setWidth/2` and `getWidth/1`.

* For columns only: the format of the column; one of `wxLIST_FORMAT_LEFT`, `wxLIST_FORMAT_RIGHT`, `wxLIST_FORMAT_CENTRE`.
See `setAlign/2` and `getAlign/1`.

* The state of the item; see `setState/2` and `getState/1`. This is a bitlist of the following flags:

* `wxLIST_STATE_FOCUSED:` The item has the focus.

* `wxLIST_STATE_SELECTED:` The item is selected.

* `wxLIST_STATE_DONTCARE:` No special flags (the value of this constant is 0).

* `wxLIST_STATE_DROPHILITED:` The item is highlighted to receive a drop event. Win32 only.

* `wxLIST_STATE_CUT:` The item is in the cut state. Win32 only.

* A mask indicating which state flags are valid; this is a bitlist of the flags reported
above for the item state. See `setStateMask/2` and GetStateMask().

* A mask indicating which fields of this class are valid; see `setMask/2` and `getMask/1`. This is a bitlist of
the following flags:

* `wxLIST_MASK_STATE:` The state field is valid.

* `wxLIST_MASK_TEXT:` The label field is valid.

* `wxLIST_MASK_IMAGE:` The image field is valid.

* `wxLIST_MASK_DATA:` The application-defined data field is valid.

* `wxLIST_MASK_WIDTH:` The column width field is valid.

* `wxLIST_MASK_FORMAT:` The column format field is valid.

The `m:wxListItem` object can also contain item-specific colour and font information: for
this you need to call one of `setTextColour/2`, `setBackgroundColour/2` or `setFont/2` functions on it passing it the colour/font to use. If
the colour/font is not specified, the default list control colour/font is used.

See: `m:wxListCtrl`

wxWidgets docs: [wxListItem](https://docs.wxwidgets.org/3.2/classwx_list_item.html)

# `wxListItem`

```erlang
-type wxListItem() :: wx:wx_object().
```

# `clear`

```erlang
-spec clear(This) -> ok when This :: wxListItem().
```

Resets the item state to the default.

# `destroy`

```erlang
-spec destroy(This :: wxListItem()) -> ok.
```

Destroys the object

# `getAlign`

```erlang
-spec getAlign(This) -> wx:wx_enum() when This :: wxListItem().
```

Returns the alignment for this item.

Can be one of `wxLIST_FORMAT_LEFT`, `wxLIST_FORMAT_RIGHT` or `wxLIST_FORMAT_CENTRE`.

# `getBackgroundColour`

```erlang
-spec getBackgroundColour(This) -> wx:wx_colour4() when This :: wxListItem().
```

Returns the background colour for this item.

# `getColumn`

```erlang
-spec getColumn(This) -> integer() when This :: wxListItem().
```

Returns the zero-based column; meaningful only in report mode.

# `getFont`

```erlang
-spec getFont(This) -> wxFont:wxFont() when This :: wxListItem().
```

Returns the font used to display the item.

# `getId`

```erlang
-spec getId(This) -> integer() when This :: wxListItem().
```

Returns the zero-based item position.

# `getImage`

```erlang
-spec getImage(This) -> integer() when This :: wxListItem().
```

Returns the zero-based index of the image associated with the item into the image list.

# `getMask`

```erlang
-spec getMask(This) -> integer() when This :: wxListItem().
```

Returns a bit mask indicating which fields of the structure are valid.

Can be any combination of the following values:

* wxLIST_MASK_STATE: `GetState` is valid.

* wxLIST_MASK_TEXT: `GetText` is valid.

* wxLIST_MASK_IMAGE: `GetImage` is valid.

* wxLIST_MASK_DATA: `GetData` is valid.

* wxLIST_MASK_WIDTH: `GetWidth` is valid.

* wxLIST_MASK_FORMAT: `GetFormat` is valid.

# `getState`

```erlang
-spec getState(This) -> integer() when This :: wxListItem().
```

Returns a bit field representing the state of the item.

Can be any combination of:

* wxLIST_STATE_DONTCARE: No special flags (the values of this constant is 0).

* wxLIST_STATE_DROPHILITED: The item is highlighted to receive a drop event. Win32 only.

* wxLIST_STATE_FOCUSED: The item has the focus.

* wxLIST_STATE_SELECTED: The item is selected.

* wxLIST_STATE_CUT: The item is in the cut state. Win32 only.

# `getText`

```erlang
-spec getText(This) -> unicode:charlist() when This :: wxListItem().
```

Returns the label/header text.

# `getTextColour`

```erlang
-spec getTextColour(This) -> wx:wx_colour4() when This :: wxListItem().
```

Returns the text colour.

# `getWidth`

```erlang
-spec getWidth(This) -> integer() when This :: wxListItem().
```

Meaningful only for column headers in report mode.

Returns the column width.

# `new`

```erlang
-spec new() -> wxListItem().
```

Constructor.

# `new`

```erlang
-spec new(Item) -> wxListItem() when Item :: wxListItem().
```

# `setAlign`

```erlang
-spec setAlign(This, Align) -> ok when This :: wxListItem(), Align :: wx:wx_enum().
```

Sets the alignment for the item.

See also `getAlign/1`

# `setBackgroundColour`

```erlang
-spec setBackgroundColour(This, ColBack) -> ok when This :: wxListItem(), ColBack :: wx:wx_colour().
```

Sets the background colour for the item.

# `setColumn`

```erlang
-spec setColumn(This, Col) -> ok when This :: wxListItem(), Col :: integer().
```

Sets the zero-based column.

Meaningful only in report mode.

# `setFont`

```erlang
-spec setFont(This, Font) -> ok when This :: wxListItem(), Font :: wxFont:wxFont().
```

Sets the font for the item.

# `setId`

```erlang
-spec setId(This, Id) -> ok when This :: wxListItem(), Id :: integer().
```

Sets the zero-based item position.

# `setImage`

```erlang
-spec setImage(This, Image) -> ok when This :: wxListItem(), Image :: integer().
```

Sets the zero-based index of the image associated with the item into the image list.

# `setMask`

```erlang
-spec setMask(This, Mask) -> ok when This :: wxListItem(), Mask :: integer().
```

Sets the mask of valid fields.

See `getMask/1`.

# `setState`

```erlang
-spec setState(This, State) -> ok when This :: wxListItem(), State :: integer().
```

Sets the item state flags (note that the valid state flags are influenced by the value of
the state mask, see `setStateMask/2`).

See `getState/1` for valid flag values.

# `setStateMask`

```erlang
-spec setStateMask(This, StateMask) -> ok when This :: wxListItem(), StateMask :: integer().
```

Sets the bitmask that is used to determine which of the state flags are to be set.

See also `setState/2`.

# `setText`

```erlang
-spec setText(This, Text) -> ok when This :: wxListItem(), Text :: unicode:chardata().
```

Sets the text label for the item.

# `setTextColour`

```erlang
-spec setTextColour(This, ColText) -> ok when This :: wxListItem(), ColText :: wx:wx_colour().
```

Sets the text colour for the item.

# `setWidth`

```erlang
-spec setWidth(This, Width) -> ok when This :: wxListItem(), Width :: integer().
```

Meaningful only for column headers in report mode.

Sets the column width.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
