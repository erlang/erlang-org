# `wxRadioBox`
[🔗](https://github.com/erlang/otp/blob/OTP-28.5.0.3/lib/wx/src/gen/wxRadioBox.erl#L58)

A radio box item is used to select one of number of mutually exclusive choices.

It is displayed as a vertical column or horizontal row of labelled buttons.

## Styles

This class supports the following styles:

* wxRA_SPECIFY_ROWS: The major dimension parameter refers to the maximum number of rows.

* wxRA_SPECIFY_COLS: The major dimension parameter refers to the maximum number of columns.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxRadioButton`

* `m:wxCheckBox`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxRadioBox](https://docs.wxwidgets.org/3.2/classwx_radio_box.html)

## Events

Event types emitted from this class:

* [`command_radiobox_selected`](`m:wxCommandEvent`)

# `wxRadioBox`

```elixir
-type wxRadioBox() :: wx:wx_object().
```

# `create`

```elixir
-spec create(This, Parent, Id, Label, Pos, Size, Choices) -> boolean()
                when
                    This :: wxRadioBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()].
```

# `create`

```elixir
-spec create(This, Parent, Id, Label, Pos, Size, Choices, [Option]) -> boolean()
                when
                    This :: wxRadioBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Label :: unicode:chardata(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()],
                    Option :: {majorDim, integer()} | {style, integer()} | {val, wx:wx_object()}.
```

Creates the radiobox for two-step construction.

See `new/7` for further details.

# `destroy`

```elixir
-spec destroy(This :: wxRadioBox()) -> ok.
```

Destroys the object

# `enable`

```elixir
-spec enable(This) -> boolean() when This :: wxRadioBox().
```

# `enable`

```elixir
-spec enable(This, N) -> boolean() when This :: wxRadioBox(), N :: integer();
            (This, [Option]) -> boolean() when This :: wxRadioBox(), Option :: {enable, boolean()}.
```

Enables or disables the radiobox.

See: `wxWindow:enable/2`

# `enable`

```elixir
-spec enable(This, N, [Option]) -> boolean()
                when This :: wxRadioBox(), N :: integer(), Option :: {enable, boolean()}.
```

Enables or disables an individual button in the radiobox.

See: `wxWindow:enable/2`

# `getColumnCount`

```elixir
-spec getColumnCount(This) -> integer() when This :: wxRadioBox().
```

Returns the number of columns in the radiobox.

# `getItemFromPoint`

```elixir
-spec getItemFromPoint(This, Pt) -> integer()
                          when This :: wxRadioBox(), Pt :: {X :: integer(), Y :: integer()}.
```

Returns a radio box item under the point, a zero-based item index, or `wxNOT\_FOUND` if
no item is under the point.

# `getItemHelpText`

```elixir
-spec getItemHelpText(This, Item) -> unicode:charlist() when This :: wxRadioBox(), Item :: integer().
```

Returns the helptext associated with the specified `item` if any or `wxEmptyString`.

See: `setItemHelpText/3`

# `getItemToolTip`

```elixir
-spec getItemToolTip(This, Item) -> wxToolTip:wxToolTip() when This :: wxRadioBox(), Item :: integer().
```

Returns the tooltip associated with the specified `item` if any or NULL.

See:
* `setItemToolTip/3`

* `wxWindow:getToolTip/1`

# `getRowCount`

```elixir
-spec getRowCount(This) -> integer() when This :: wxRadioBox().
```

Returns the number of rows in the radiobox.

# `getSelection`

```elixir
-spec getSelection(This) -> integer() when This :: wxRadioBox().
```

Returns the index of the selected item or `wxNOT\_FOUND` if no item is selected.

Return: The position of the current selection.

Remark: This method can be used with single selection list boxes only, you should use `wxListBox:getSelections/1`
for the list boxes with wxLB_MULTIPLE style.

See:
* `setSelection/2`

* `wxControlWithItems:getStringSelection/1`

# `getString`

```elixir
-spec getString(This, N) -> unicode:charlist() when This :: wxRadioBox(), N :: integer().
```

Returns the label of the item with the given index.

Return: The label of the item or an empty string if the position was invalid.

# `isItemEnabled`

```elixir
-spec isItemEnabled(This, N) -> boolean() when This :: wxRadioBox(), N :: integer().
```

Returns true if the item is enabled or false if it was disabled using `enable/3`.

This function is currently only implemented in wxMSW, wxGTK, wxQT and wxUniversal and
always returns true in the other ports.

# `isItemShown`

```elixir
-spec isItemShown(This, N) -> boolean() when This :: wxRadioBox(), N :: integer().
```

Returns true if the item is currently shown or false if it was hidden using `show/3`.

Note that this function returns true for an item which hadn't been hidden even if the
entire radiobox is not currently shown.

This function is currently only implemented in wxMSW, wxGTK, wxQT and wxUniversal and
always returns true in the other ports.

# `new`

```elixir
-spec new(Parent, Id, Label, Pos, Size, Choices) -> wxRadioBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: unicode:chardata(),
                 Pos :: {X :: integer(), Y :: integer()},
                 Size :: {W :: integer(), H :: integer()},
                 Choices :: [unicode:chardata()].
```

# `new`

```elixir
-spec new(Parent, Id, Label, Pos, Size, Choices, [Option]) -> wxRadioBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Label :: unicode:chardata(),
                 Pos :: {X :: integer(), Y :: integer()},
                 Size :: {W :: integer(), H :: integer()},
                 Choices :: [unicode:chardata()],
                 Option :: {majorDim, integer()} | {style, integer()} | {val, wx:wx_object()}.
```

Constructor, creating and showing a radiobox.

See: `create/8`

# `setItemHelpText`

```elixir
-spec setItemHelpText(This, Item, Helptext) -> ok
                         when This :: wxRadioBox(), Item :: integer(), Helptext :: unicode:chardata().
```

Sets the helptext for an item.

Empty string erases any existing helptext.

See: `getItemHelpText/2`

# `setItemToolTip`

```elixir
-spec setItemToolTip(This, Item, Text) -> ok
                        when This :: wxRadioBox(), Item :: integer(), Text :: unicode:chardata().
```

Sets the tooltip text for the specified item in the radio group.

This function is currently only implemented in wxMSW and wxGTK2 and does nothing in the
other ports.

See:
* `getItemToolTip/2`

* `wxWindow:setToolTip/2`

# `setSelection`

```elixir
-spec setSelection(This, N) -> ok when This :: wxRadioBox(), N :: integer().
```

Sets the selection to the given item.

Notice that a radio box always has selection, so `n` must be valid here and passing `wxNOT_FOUND`
is not allowed.

# `show`

```elixir
-spec show(This, Item) -> boolean() when This :: wxRadioBox(), Item :: integer().
```

# `show`

```elixir
-spec show(This, Item, [Option]) -> boolean()
              when This :: wxRadioBox(), Item :: integer(), Option :: {show, boolean()}.
```

Shows or hides individual buttons.

Return: true if the item has been shown or hidden or false if nothing was done because it
already was in the requested state.

See: `show/3`

---

*Consult [api-reference.md](api-reference.md) for complete listing*
