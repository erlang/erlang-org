# `wxListBox`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxListBox.erl#L58)

A listbox is used to select one or more of a list of strings.

The strings are displayed in a scrolling box, with the selected string(s) marked in
reverse video. A listbox can be single selection (if an item is selected, the previous
selection is removed) or multiple selection (clicking an item toggles the item on or off
independently of other selections).

List box elements are numbered from zero and while the maximal number of elements is
unlimited, it is usually better to use a virtual control, not requiring to add all the
items to it at once, such as `wxDataViewCtrl` (not implemented in wx) or `m:wxListCtrl`
with `wxLC_VIRTUAL` style, once more than a few hundreds items need to be displayed
because this control is not optimized, neither from performance nor from user interface
point of view, for large number of items.

Notice that the list box doesn't support control characters other than `TAB`.

## Styles

This class supports the following styles:

* wxLB_SINGLE: Single-selection list.

* wxLB_MULTIPLE: Multiple-selection list: the user can toggle multiple items on and off.
This is the same as wxLB_EXTENDED in wxGTK2 port.

* wxLB_EXTENDED: Extended-selection list: the user can extend the selection by using `SHIFT`
or `CTRL` keys together with the cursor movement keys or the mouse.

* wxLB_HSCROLL: Create horizontal scrollbar if contents are too wide (Windows only).

* wxLB_ALWAYS_SB: Always show a vertical scrollbar.

* wxLB_NEEDED_SB: Only create a vertical scrollbar if needed.

* wxLB_NO_SB: Don't create vertical scrollbar (wxMSW and wxGTK only).

* wxLB_SORT: The listbox contents are sorted in alphabetical order. Note that `wxLB_SINGLE`, `wxLB_MULTIPLE`
and `wxLB_EXTENDED` styles are mutually exclusive and you can specify at most one of them
(single selection is the default). See also overview_windowstyles.

See:
* `m:wxChoice`

* `m:wxComboBox`

* `m:wxListCtrl`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxControlWithItems`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxListBox](https://docs.wxwidgets.org/3.2/classwx_list_box.html)

## Events

Event types emitted from this class:

* [`command_listbox_selected`](`m:wxCommandEvent`)

* [`command_listbox_doubleclicked`](`m:wxCommandEvent`)

# `wxListBox`

```erlang
-type wxListBox() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Pos, Size, Choices) -> boolean()
                when
                    This :: wxListBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()].
```

# `create`

```erlang
-spec create(This, Parent, Id, Pos, Size, Choices, [Option]) -> boolean()
                when
                    This :: wxListBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()],
                    Option :: {style, integer()} | {validator, wx:wx_object()}.
```

# `deselect`

```erlang
-spec deselect(This, N) -> ok when This :: wxListBox(), N :: integer().
```

Deselects an item in the list box.

Remark: This applies to multiple selection listboxes only.

# `destroy`

```erlang
-spec destroy(This :: wxListBox()) -> ok.
```

Destroys the object

# `getSelections`

```erlang
-spec getSelections(This) -> Result
                       when Result :: {Res :: integer(), Selections :: [integer()]}, This :: wxListBox().
```

Fill an array of ints with the positions of the currently selected items.

Return: The number of selections.

Remark: Use this with a multiple selection listbox.

See:
* `wxControlWithItems:getSelection/1`

* `wxControlWithItems:getStringSelection/1`

* `wxControlWithItems:setSelection/2`

# `hitTest`

```erlang
-spec hitTest(This, Point) -> integer()
                 when This :: wxListBox(), Point :: {X :: integer(), Y :: integer()}.
```

Returns the item located at `point`, or `wxNOT\_FOUND` if there is no item located at `point`.

It is currently implemented for wxMSW, wxMac and wxGTK2 ports.

Return: Item located at point, or wxNOT_FOUND if unimplemented or the item does not exist.

Since: 2.7.0

# `hitTest`

```erlang
-spec hitTest(This, X, Y) -> integer() when This :: wxListBox(), X :: integer(), Y :: integer().
```

This is an overloaded member function, provided for convenience. It differs from the
above function only in what argument(s) it accepts.

# `insertItems`

```erlang
-spec insertItems(This, Items, Pos) -> ok
                     when This :: wxListBox(), Items :: [unicode:chardata()], Pos :: integer().
```

Insert the given number of strings before the specified position.

# `isSelected`

```erlang
-spec isSelected(This, N) -> boolean() when This :: wxListBox(), N :: integer().
```

Determines whether an item is selected.

Return: true if the given item is selected, false otherwise.

# `new`

```erlang
-spec new() -> wxListBox().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxListBox() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxListBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {choices, [unicode:chardata()]} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a list box.

See the other `new/3` constructor; the only difference is that this overload takes a `wxArrayString`
(not implemented in wx) instead of a pointer to an array of `wxString` (not implemented
in wx).

# `set`

```erlang
-spec set(This, Items) -> ok when This :: wxListBox(), Items :: [unicode:chardata()].
```

Replaces the current control contents with the given items.

Notice that calling this method is usually much faster than appending them one by one if
you need to add a lot of items.

# `setFirstItem`

```erlang
-spec setFirstItem(This, N) -> ok when This :: wxListBox(), N :: integer();
                  (This, String) -> ok when This :: wxListBox(), String :: unicode:chardata().
```

Set the specified item to be the first visible item.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
