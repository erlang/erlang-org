# `wxComboBox`
[🔗](https://github.com/erlang/otp/blob/master/lib/wx/src/gen/wxComboBox.erl#L58)

A combobox is like a combination of an edit control and a listbox.

It can be displayed as static list with editable or read-only text field; or a drop-down
list with text field; or a drop-down list without a text field depending on the platform
and presence of wxCB_READONLY style.

A combobox permits a single selection only. Combobox items are numbered from zero.

If you need a customized combobox, have a look at `wxComboCtrl` (not implemented in wx), `wxOwnerDrawnComboBox`
(not implemented in wx), `wxComboPopup` (not implemented in wx) and the ready-to-use `wxBitmapComboBox`
(not implemented in wx).

Please refer to `wxTextEntry` (not implemented in wx) documentation for the description
of methods operating with the text entry part of the combobox and to `wxItemContainer`
(not implemented in wx) for the methods operating with the list of strings. Notice that at
least under MSW `m:wxComboBox` doesn't behave correctly if it contains strings differing
in case only so portable programs should avoid adding such strings to this control.

## Styles

This class supports the following styles:

* wxCB_SIMPLE: Creates a combobox with a permanently displayed list. Windows only.

* wxCB_DROPDOWN: Creates a combobox with a drop-down list. MSW and Motif only.

* wxCB_READONLY: A combobox with this style behaves like a `m:wxChoice` (and may look in
the same way as well, although this is platform-dependent), i.e. it allows the user to
choose from the list of options but doesn't allow to enter a value not present in the
list.

* wxCB_SORT: Sorts the entries in the list alphabetically.

* wxTE_PROCESS_ENTER: The control will generate the event `wxEVT_TEXT_ENTER` that can be
handled by the program. Otherwise, i.e. either if this style not specified at all, or it
is used, but there is no event handler for this event or the event handler called `wxEvent:skip/2` to
avoid overriding the default handling, pressing Enter key is either processed internally
by the control or used to activate the default button of the dialog, if any.

See:
* `m:wxListBox`

* `m:wxTextCtrl`

* `m:wxChoice`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxControlWithItems`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxComboBox](https://docs.wxwidgets.org/3.2/classwx_combo_box.html)

## Events

Event types emitted from this class:

* [`command_combobox_selected`](`m:wxCommandEvent`)

* [`command_text_updated`](`m:wxCommandEvent`)

* [`command_text_enter`](`m:wxCommandEvent`)

* [`combobox_dropdown`](`m:wxCommandEvent`)

* [`combobox_closeup`](`m:wxCommandEvent`)

# `wxComboBox`

```erlang
-type wxComboBox() :: wx:wx_object().
```

# `canCopy`

```erlang
-spec canCopy(This) -> boolean() when This :: wxComboBox().
```

Returns true if the selection can be copied to the clipboard.

# `canCut`

```erlang
-spec canCut(This) -> boolean() when This :: wxComboBox().
```

Returns true if the selection can be cut to the clipboard.

# `canPaste`

```erlang
-spec canPaste(This) -> boolean() when This :: wxComboBox().
```

Returns true if the contents of the clipboard can be pasted into the text control.

On some platforms (Motif, GTK) this is an approximation and returns true if the control
is editable, false otherwise.

# `canRedo`

```erlang
-spec canRedo(This) -> boolean() when This :: wxComboBox().
```

Returns true if there is a redo facility available and the last operation can be redone.

# `canUndo`

```erlang
-spec canUndo(This) -> boolean() when This :: wxComboBox().
```

Returns true if there is an undo facility available and the last operation can be undone.

# `copy`

```erlang
-spec copy(This) -> ok when This :: wxComboBox().
```

Copies the selected text to the clipboard.

# `create`

```erlang
-spec create(This, Parent, Id, Value, Pos, Size, Choices) -> boolean()
                when
                    This :: wxComboBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Value :: unicode:chardata(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()].
```

# `create`

```erlang
-spec create(This, Parent, Id, Value, Pos, Size, Choices, [Option]) -> boolean()
                when
                    This :: wxComboBox(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Value :: unicode:chardata(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()],
                    Option :: {style, integer()} | {validator, wx:wx_object()}.
```

# `cut`

```erlang
-spec cut(This) -> ok when This :: wxComboBox().
```

Copies the selected text to the clipboard and removes it from the control.

# `destroy`

```erlang
-spec destroy(This :: wxComboBox()) -> ok.
```

Destroys the object

# `getInsertionPoint`

```erlang
-spec getInsertionPoint(This) -> integer() when This :: wxComboBox().
```

Same as `wxTextCtrl:getInsertionPoint/1`.

Note: Under wxMSW, this function always returns 0 if the combobox doesn't have the focus.

# `getLastPosition`

```erlang
-spec getLastPosition(This) -> integer() when This :: wxComboBox().
```

Returns the zero based index of the last position in the text control, which is equal to
the number of characters in the control.

# `getValue`

```erlang
-spec getValue(This) -> unicode:charlist() when This :: wxComboBox().
```

Gets the contents of the control.

Notice that for a multiline text control, the lines will be separated by (Unix-style) `\n`
characters, even under Windows where they are separated by a `\r\n` sequence in the
native control.

# `new`

```erlang
-spec new() -> wxComboBox().
```

Default constructor.

# `new`

```erlang
-spec new(Parent, Id) -> wxComboBox() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxComboBox()
             when
                 Parent :: wxWindow:wxWindow(),
                 Id :: integer(),
                 Option ::
                     {value, unicode:chardata()} |
                     {pos, {X :: integer(), Y :: integer()}} |
                     {size, {W :: integer(), H :: integer()}} |
                     {choices, [unicode:chardata()]} |
                     {style, integer()} |
                     {validator, wx:wx_object()}.
```

Constructor, creating and showing a combobox.

See: `create/8`

# `paste`

```erlang
-spec paste(This) -> ok when This :: wxComboBox().
```

Pastes text from the clipboard to the text item.

# `redo`

```erlang
-spec redo(This) -> ok when This :: wxComboBox().
```

If there is a redo facility and the last operation can be redone, redoes the last
operation.

Does nothing if there is no redo facility.

# `remove`

```erlang
-spec remove(This, From, To) -> ok when This :: wxComboBox(), From :: integer(), To :: integer().
```

Removes the text starting at the first given position up to (but not including) the
character at the last position.

This function puts the current insertion point position at `to` as a side effect.

# `replace`

```erlang
-spec replace(This, From, To, Value) -> ok
                 when
                     This :: wxComboBox(),
                     From :: integer(),
                     To :: integer(),
                     Value :: unicode:chardata().
```

Replaces the text starting at the first position up to (but not including) the character
at the last position with the given text.

This function puts the current insertion point position at `to` as a side effect.

# `setInsertionPoint`

```erlang
-spec setInsertionPoint(This, Pos) -> ok when This :: wxComboBox(), Pos :: integer().
```

Sets the insertion point at the given position.

# `setInsertionPointEnd`

```erlang
-spec setInsertionPointEnd(This) -> ok when This :: wxComboBox().
```

Sets the insertion point at the end of the text control.

This is equivalent to calling `setInsertionPoint/2` with `getLastPosition/1` argument.

# `setSelection`

```erlang
-spec setSelection(This, N) -> ok when This :: wxComboBox(), N :: integer().
```

Sets the selection to the given item `n` or removes the selection entirely if `n` == `wxNOT\_FOUND`.

Note that this does not cause any command events to be emitted nor does it deselect any
other items in the controls which support multiple selections.

See:
* `wxControlWithItems:setString/3`

* `wxControlWithItems:setStringSelection/2`

# `setSelection`

```erlang
-spec setSelection(This, From, To) -> ok when This :: wxComboBox(), From :: integer(), To :: integer().
```

Same as `wxTextCtrl:setSelection/3`.

# `setValue`

```erlang
-spec setValue(This, Text) -> ok when This :: wxComboBox(), Text :: unicode:chardata().
```

Sets the text for the combobox text field.

For normal, editable comboboxes with a text entry field calling this method will generate
a `wxEVT_TEXT` event, consistently with `wxTextCtrl:setValue/2` behaviour, use `wxTextCtrl:changeValue/2` if this is undesirable.

For controls with `wxCB_READONLY` style the method behaves somewhat differently: the
string must be in the combobox choices list (the check for this is case-insensitive) and `wxEVT_TEXT`
is `not` generated in this case.

# `undo`

```erlang
-spec undo(This) -> ok when This :: wxComboBox().
```

If there is an undo facility and the last operation can be undone, undoes the last
operation.

Does nothing if there is no undo facility.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
