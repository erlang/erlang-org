# `wxChoice`
[🔗](https://github.com/erlang/otp/blob/OTP-29.0.3/lib/wx/src/gen/wxChoice.erl#L58)

A choice item is used to select one of a list of strings.

Unlike a `m:wxListBox`, only the selection is visible until the user pulls down the menu
of choices.

## Styles

This class supports the following styles:

* wxCB_SORT: Sorts the entries alphabetically.

See:
* `m:wxListBox`

* `m:wxComboBox`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxControlWithItems`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxChoice](https://docs.wxwidgets.org/3.2/classwx_choice.html)

## Events

Event types emitted from this class:

* [`command_choice_selected`](`m:wxCommandEvent`)

# `wxChoice`

```erlang
-type wxChoice() :: wx:wx_object().
```

# `create`

```erlang
-spec create(This, Parent, Id, Pos, Size, Choices) -> boolean()
                when
                    This :: wxChoice(),
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
                    This :: wxChoice(),
                    Parent :: wxWindow:wxWindow(),
                    Id :: integer(),
                    Pos :: {X :: integer(), Y :: integer()},
                    Size :: {W :: integer(), H :: integer()},
                    Choices :: [unicode:chardata()],
                    Option :: {style, integer()} | {validator, wx:wx_object()}.
```

# `delete`

```erlang
-spec delete(This, N) -> ok when This :: wxChoice(), N :: integer().
```

Deletes an item from the control.

The client data associated with the item will be also deleted if it is owned by the
control. Note that it is an error (signalled by an assert failure in debug builds) to
remove an item with the index negative or greater or equal than the number of items in the control.

If there is a currently selected item below the item being deleted, i.e. if `wxControlWithItems:getSelection/1` returns a
valid index greater than or equal to `n`, the selection is invalidated when this function
is called. However if the selected item appears before the item being deleted, the
selection is preserved unchanged.

See: `wxControlWithItems:clear/1`

# `destroy`

```erlang
-spec destroy(This :: wxChoice()) -> ok.
```

Destroys the object

# `getColumns`

```erlang
-spec getColumns(This) -> integer() when This :: wxChoice().
```

Gets the number of columns in this choice item.

Remark: This is implemented for GTK and Motif only and always returns 1 for the other
platforms.

# `new`

```erlang
-spec new() -> wxChoice().
```

Default constructor.

See: `create/7`

# `new`

```erlang
-spec new(Parent, Id) -> wxChoice() when Parent :: wxWindow:wxWindow(), Id :: integer().
```

# `new`

```erlang
-spec new(Parent, Id, [Option]) -> wxChoice()
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

Constructor, creating and showing a choice.

See: `create/7`

# `setColumns`

```erlang
-spec setColumns(This) -> ok when This :: wxChoice().
```

# `setColumns`

```erlang
-spec setColumns(This, [Option]) -> ok when This :: wxChoice(), Option :: {n, integer()}.
```

Sets the number of columns in this choice item.

Remark: This is implemented for GTK and Motif only and doesn’t do anything under other
platforms.

---

*Consult [api-reference.md](api-reference.md) for complete listing*
